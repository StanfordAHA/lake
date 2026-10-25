import kratos as kts
from lake.utils.spec_enum import Direction
from lake.spec.schedule_generator import ScheduleGenerator
from lake.modules.lf_comp_block import LFCompBlock, LFComparisonOperator
from lake.utils.util import inline_multiplexer
from lake.spec.component import Component

# The bitstream converter's barrier: a RAW (LT) scalar no counter pair can
# satisfy, so the reader passes only once a side finishes.
RV_BARRIER_SCALAR = 16383

# rv_comparator_trim=True (Spec / build_spec_rv / build_pond_rv / RV pond spec
# key): one-sided compare per direction, 8-bit scalar + barrier bit, level
# select over levels 0..2. Every constraint of the 2026-10-11 app audit fits
# (6,190 constraints: non-barrier scalars -127..36, levels 0..2).
RV_CMP_TRIM_DEFAULT = {"one_sided": True, "scalar_width": 8, "barrier_bit": True, "max_level": 3}
RV_CMP_TRIM_KEYS = set(RV_CMP_TRIM_DEFAULT)


def resolve_rv_comparator_trim(trim):
    """None / False -> {} (the full comparator); True -> RV_CMP_TRIM_DEFAULT;
    a dict -> those RVComparisonNetwork options (unknown keys raise)."""
    if trim is None or trim is False:
        return {}
    if trim is True:
        return dict(RV_CMP_TRIM_DEFAULT)
    if not isinstance(trim, dict):
        raise ValueError(f"rv_comparator_trim must be a bool or a dict, not {trim!r}")
    unknown = set(trim) - RV_CMP_TRIM_KEYS
    if unknown:
        raise ValueError(f"rv_comparator_trim: unknown keys {sorted(unknown)} (takes {sorted(RV_CMP_TRIM_KEYS)})")
    return dict(trim)


class RVComparisonNetwork(Component):

    def __init__(self, name: str, debug: bool = False, is_clone: bool = False, internal_generator=None,
                 writes=None, reads=None, prune_cross_group=False,
                 one_sided=False, scalar_width=16, barrier_bit=False, max_level=None):
        """Trim options (defaults = the full network, unchanged RTL):
          one_sided: each LFCompBlock has its direction's op built in (reader
            waits on writer = LT, writer waits on reader = GT), see LFCompBlock.
          scalar_width: signed comparison_scalar width.
          barrier_bit: a barrier register per block; gen_bitstream programs a
            barrier constraint (LT scalar >= RV_BARRIER_SCALAR) with it, so
            its levels and scalar need no hardware.
          max_level: a port's level select covers levels 0..max_level-1 (the
            wrap fixup reads up to level max_level); deeper non-barrier
            constraints raise in gen_bitstream."""
        super().__init__(name=name)
        if max_level is not None and max_level < 1:
            raise ValueError(f"RVComparisonNetwork: max_level {max_level} < 1")
        self.one_sided = one_sided
        self.scalar_width = scalar_width
        self.barrier_bit = barrier_bit
        self.max_level = max_level
        # number of selectable levels (writer side, reader side) per block key
        self.levels_map = {}
        # super().__init__(name, debug, is_clone, internal_generator)
        self.writes = []
        self.reads = []
        if writes is not None:
            self.writes.extend(writes)
        if reads is not None:
            self.reads.extend(reads)
        # Storage group of each writer/reader (None = unknown, pairs with everything).
        # With prune_cross_group, a writer/reader pair from different groups gets no
        # LFCompBlock, muxes or select registers: its comparison bit is tied to 1,
        # what a disabled block outputs. Spec passes each port's storage index;
        # its bitstream converter already rejects cross-storage dependences (the
        # data never meets), so those blocks could never be enabled.
        self.write_groups = [None] * len(self.writes)
        self.read_groups = [None] * len(self.reads)
        self.prune_cross_group = prune_cross_group
        self.rw_div = None

        self.lfcs = {}

        # self.in_sels_wr_to_rd = []
        # self.out_sels_wr_to_rd = []
        # self.in_sels_rd_to_wr = []
        # self.out_sels_rd_to_wr = []

        self.sels_map = {}

        self.connections = []

    def add_reader_writer(self, direction: Direction, sg: ScheduleGenerator, group=None):
        # Based on the direction, register this with either self.writes or self.reads
        if direction == Direction.IN:
            self.writes.append(sg)
            self.write_groups.append(group)
        elif direction == Direction.OUT:
            self.reads.append(sg)
            self.read_groups.append(group)
        else:
            raise NotImplementedError

    def pair_exists(self, write_idx, read_idx):
        """Whether writer write_idx and reader read_idx get comparators."""
        if not self.prune_cross_group:
            return True
        wg, rg = self.write_groups[write_idx], self.read_groups[read_idx]
        return wg is None or rg is None or wg == rg

    def num_comparators(self):
        return 2 * sum(self.pair_exists(i, j) for i in range(len(self.writes)) for j in range(len(self.reads)))

    def get_connections(self):
        return self.connections

    def get_port_from_index(self, p_idx: int):

        assert self.rw_div is not None
        if p_idx < self.rw_div:
            return self.writes[p_idx]
        else:
            return self.reads[p_idx - self.rw_div]

    def get_mux_sel_reg_from_indexes(self, p1, p2):

        adj_p1_idx = None
        adj_p2_idx = None
        rd_to_wr = None
        # Check if it is read to write or write to read by evaluating the p1
        if p1 < self.rw_div:
            assert p2 >= self.rw_div
            adj_p1_idx = p1
            adj_p2_idx = p2 - self.rw_div
            rd_to_wr = 'wr_to_rd'
        elif p1 >= self.rw_div:
            assert p2 < self.rw_div
            adj_p1_idx = p1 - self.rw_div
            adj_p2_idx = p2
            rd_to_wr = 'rd_to_wr'

        key = f"{rd_to_wr}_{adj_p1_idx}_{adj_p2_idx}"
        if key not in self.sels_map:
            raise ValueError(f"RV comparison network: no comparator for {key} (ports on different storages)")
        return self.sels_map[key]
        # return in_list[adj_p1_idx], out_list[adj_p2_idx]

    def get_lfc_from_indexes(self, p1, p2):

        adj_p1_idx = None
        adj_p2_idx = None
        rd_to_wr = None

        if p1 < self.rw_div:
            assert p2 >= self.rw_div
            adj_p1_idx = p1
            adj_p2_idx = p2 - self.rw_div
            rd_to_wr = 'wr_to_rd'
        elif p1 >= self.rw_div:
            assert p2 < self.rw_div
            adj_p1_idx = p1 - self.rw_div
            adj_p2_idx = p2
            rd_to_wr = 'rd_to_wr'

        key = f"{rd_to_wr}_{adj_p1_idx}_{adj_p2_idx}"
        if key not in self.lfcs:
            raise ValueError(f"RV comparison network: no comparator for {key} (ports on different storages)")
        return self.lfcs[key]

    def _num_sel_levels(self, dims):
        return dims if self.max_level is None else min(self.max_level, dims)

    @staticmethod
    def _sel_levels(iters, nsel, dims):
        """The mux inputs for levels 0..nsel-1 (the whole array untrimmed)."""
        return iters if nsel == dims else [iters[lvl] for lvl in range(nsel)]

    def _lf_block(self, op, in_width, out_width):
        trim = {}
        if self.one_sided:
            trim["one_sided_op"] = op
        if self.scalar_width != 16:
            trim["scalar_width"] = self.scalar_width
        if self.barrier_bit:
            trim["barrier_bit"] = True
        return LFCompBlock(name='lf_comp_block', in_width=in_width, out_width=out_width, **trim)

    def _outer(self, name, mux_name, sel, iters, dims, width, nsel):
        """_outer_counter, or with a trimmed select (nsel < dims): the
        iterator of level sel + 1 from a mux over levels 1..nsel indexed by
        sel itself (every one exists, since nsel < dims)."""
        if nsel == dims:
            return self._outer_counter(name, mux_name, sel, iters, dims, width)
        out = self.var(name=name, width=width)
        inline_multiplexer(generator=self, name=mux_name, sel=sel, one=out,
                           many=[iters[lvl + 1] for lvl in range(nsel)], one_hot_sel=False)
        return out

    def _outer_counter(self, name, mux_name, sel, iters, dims, width):
        """The port's iterator one level above the selected level `sel` (for
        the wrap fixup); 0 above the top level, where the mux select runs out
        of range. `sel + 1` is computed in one more bit when clog2(dims) bits
        cannot hold `dims` (power-of-2 dims >= 2): it used to wrap to level 0
        at the top level, so the fixup fired whenever the two ports' level-0
        iterators differed (2026-10-08). Other dims: RTL unchanged."""
        out = self.var(name=name, width=width)
        need = kts.clog2(dims + 1)
        if need > sel.width:
            sel_p1 = kts.concat(kts.const(0, need - sel.width), sel) + 1
        else:
            sel_p1 = sel + 1
        inline_multiplexer(generator=self, name=mux_name, sel=sel_p1, one=out, many=iters,
                           one_hot_sel=False)
        return out

    def gen_hardware(self):

        # Let's assume that the ports involved here are fixed at this point...
        # So we can combine the writes and reads into a 'single' list
        self.rw_div = len(self.writes)
        # Build out the interfaces first - each writer will get a number of comparisons and so will reads
        self._writer_comparisons = [self.output(f"write_{i}_comparisons", len(self.reads)) for i in range(len(self.writes))]
        self._writer_iterators = []
        self._writer_extents = []
        self._writer_finished = []
        for i in range(len(self.writes)):
            writer_sg = self.writes[i]
            writer_sg_iter_intf = writer_sg.get_iterator_intf()
            iterators_ = writer_sg_iter_intf['iterators']
            comparisons_ = writer_sg_iter_intf['comparisons']
            extents_ = writer_sg_iter_intf['extents']
            finisheds_ = writer_sg_iter_intf['finished']
            dim_ = writer_sg.get_dimensionality()
            iter_width = iterators_.width
            self._writer_iterators.append(self.input(f"write_{i}_iterators", iter_width, size=dim_, packed=True, explicit_array=True))
            self._writer_extents.append(self.input(f"write_{i}_extents", iter_width, size=dim_, packed=True, explicit_array=True))
            self._writer_finished.append(self.input(f"write_{i}_finished", 1))
            self.connections.append((iterators_, self._writer_iterators[i]))
            self.connections.append((comparisons_, self._writer_comparisons[i]))
            self.connections.append((extents_, self._writer_extents[i]))
            self.connections.append((finisheds_, self._writer_finished[i]))

        self._reader_comparisons = [self.output(f"read_{i}_comparisons", len(self.writes)) for i in range(len(self.reads))]
        self._reader_iterators = []
        self._reader_extents = []
        self._reader_finished = []
        for i in range(len(self.reads)):
            reader_sg = self.reads[i]
            reader_sg_iter_intf = reader_sg.get_iterator_intf()
            iterators_ = reader_sg_iter_intf['iterators']
            comparisons_ = reader_sg_iter_intf['comparisons']
            extents_ = reader_sg_iter_intf['extents']
            finisheds_ = reader_sg_iter_intf['finished']
            dim_ = reader_sg.get_dimensionality()
            iter_width = iterators_.width
            self._reader_iterators.append(self.input(f"read_{i}_iterators", iter_width, size=dim_, packed=True, explicit_array=True))
            self._reader_extents.append(self.input(f"read_{i}_extents", iter_width, size=dim_, packed=True, explicit_array=True))
            self._reader_finished.append(self.input(f"read_{i}_finished", 1))
            self.connections.append((iterators_, self._reader_iterators[i]))
            self.connections.append((comparisons_, self._reader_comparisons[i]))
            self.connections.append((extents_, self._reader_extents[i]))
            self.connections.append((finisheds_, self._reader_finished[i]))

        # Now, for every writer, we will build a LF block for every reader, same for the other way around (W->R)
        for i in range(len(self.writes)):
            write_sg = self.writes[i]
            write_iters = self._writer_iterators[i]
            write_extents = self._writer_extents[i]
            write_finisheds = self._writer_finished[i]
            write_width = write_iters.width
            num_write_ctrs = write_sg.get_dimensionality()
            nsel_w = self._num_sel_levels(num_write_ctrs)
            num_write_ctrs_bw = max(1, kts.clog2(nsel_w))
            # Config register!
            # in_sel = self.config_reg(name=f"write_{i}_to_read_sel", width=num_write_ctrs_bw)
            # in_sel = kts.const(0, num_write_ctrs_bw)

            for j in range(len(self.reads)):
                if not self.pair_exists(i, j):
                    self.wire(self._writer_comparisons[i][j], kts.const(1, 1))
                    continue
                in_sel = self.config_reg(name=f"write_{i}_to_read_{j}_sel_in", width=num_write_ctrs_bw)
                # self.in_sels_wr_to_rd.append(in_sel)
                read_sg = self.reads[j]
                read_iters = self._reader_iterators[j]
                read_extents = self._reader_extents[j]
                read_finisheds = self._reader_finished[j]
                # read_iters = read_sg.get_iterator_intf()['iterators']
                read_width = read_iters.width
                num_read_ctrs = read_sg.get_dimensionality()
                nsel_r = self._num_sel_levels(num_read_ctrs)
                num_read_ctrs_bw = max(1, kts.clog2(nsel_r))

                out_sel = self.config_reg(name=f"write_{i}_to_read_{j}_sel_out", width=num_read_ctrs_bw)

                # Instantiate LF block
                lf_comp_block = self._lf_block(LFComparisonOperator.GT, write_width, read_width)
                self.lfcs[f"wr_to_rd_{i}_{j}"] = lf_comp_block
                self.levels_map[f"wr_to_rd_{i}_{j}"] = (nsel_w, nsel_r)
                # Now have lf block, need to instantiate it, then wire up a mux based on the config
                lf_comp_block.gen_hardware()
                self.add_child(f"lfcompblock_w_{i}_r_{j}", lf_comp_block,
                               clk=self._clk, rst_n=self._rst_n,
                               clk_en=self._clk_en, flush=self._flush)
                lf_comp_block_intfs = lf_comp_block.get_interfaces()
                # the ith writer block, compared against jth reader
                self.wire(self._writer_comparisons[i][j], lf_comp_block_intfs['comparison'])

                # To handle overflow, we need to also consider the counter value above - if they are different, we need to modulate the counter
                p1_counter_w = self._outer(f"write_{i}_to_read_{j}_ctr_in_p1", f"lfcompblock_w_{i}_r_{j}_input_mux_ctr_p1",
                                           in_sel, write_iters, num_write_ctrs, write_width, nsel_w)
                p1_counter_r = self._outer(f"write_{i}_to_read_{j}_ctr_out_p1", f"lfcompblock_w_{i}_r_{j}_output_mux_ctr_p1",
                                           out_sel, read_iters, num_read_ctrs, read_width, nsel_r)

                # I think the input extent and output extent have to be the same (?) but not for line buffer - I think we just want to add the write
                # extent always since the write always goes over all data
                input_extent = self.var(name=f"write_{i}_read_{j}_input_extent", width=write_width)
                output_extent = self.var(name=f"write_{i}_read_{j}_output_extent", width=read_width)

                # Mux in the extents of the chosen level for overflow comparison
                inline_multiplexer(generator=self, name=f"w_{i}_r_{j}_input_extent_mux", sel=in_sel, one=input_extent,
                                   many=self._sel_levels(write_extents, nsel_w, num_write_ctrs), one_hot_sel=False)

                inline_multiplexer(generator=self, name=f"w_{i}_r_{j}_output_extent_mux", sel=out_sel, one=output_extent,
                                   many=self._sel_levels(read_extents, nsel_r, num_read_ctrs), one_hot_sel=False)

                # Now mux in the proper counter and add to it the write extent if the outer layer is different
                outer_level_different = self.var(f"w_{i}_r_{j}_outer_level_different", 1)
                self.wire(outer_level_different, p1_counter_w != p1_counter_r)

                chosen_write_iter = self.var(f"write_{i}_read_{j}_chosen_write_iter", width=write_width)
                # chosen_read_iter = self.var(f"write_{i}_read_{j}_chosen_read_iter", width=read_width)

                # Finally, we need to mux in the values based on config_regs
                # mux and attach to lf comp block
                inline_multiplexer(generator=self, name=f"lfcompblock_w_{i}_r_{j}_input_mux_ctr", sel=in_sel, one=chosen_write_iter,
                                   many=self._sel_levels(write_iters, nsel_w, num_write_ctrs), one_hot_sel=False)

                self.wire(lf_comp_block_intfs['in_counter'], kts.ternary(outer_level_different, chosen_write_iter + input_extent, chosen_write_iter))

                self.wire(lf_comp_block_intfs['in_finished'], write_finisheds)
                self.wire(lf_comp_block_intfs['out_finished'], read_finisheds)

                # out_sel = kts.const(0, num_read_ctrs_bw)
                # self.out_sels_wr_to_rd.append(out_sel)
                self.sels_map[f"wr_to_rd_{i}_{j}"] = (in_sel, out_sel)
                # out_sel = kts.const(0, num_read_ctrs_bw)
                inline_multiplexer(generator=self, name=f"lfcompblock_w_{i}_r_{j}_output_mux_ctr", sel=out_sel, one=lf_comp_block_intfs['out_counter'],
                                   many=self._sel_levels(read_iters, nsel_r, num_read_ctrs), one_hot_sel=False)

        # Now - same for the other way around (R->W)
        for i in range(len(self.reads)):
            read_sg = self.reads[i]
            read_iters = self._reader_iterators[i]
            read_finisheds = self._reader_finished[i]
            read_width = read_iters.width
            read_extents = self._reader_extents[i]
            num_read_ctrs = read_sg.get_dimensionality()
            # 2026-09-30: sized from the reader's own dimensionality (was the
            # last writer's, leaked from the loop above; identical when all
            # ports have equal dimensionality, as in every spec builder)
            nsel_r = self._num_sel_levels(num_read_ctrs)
            num_rd_dims_bw = max(1, kts.clog2(nsel_r))
            # in_sel = kts.const(0, num_read_ctrs_bw)
            # in_sel = self.config_reg(name=f"read_{i}_to_write_sel", width=num_write_ctrs_bw)
            for j in range(len(self.writes)):
                if not self.pair_exists(j, i):
                    self.wire(self._reader_comparisons[i][j], kts.const(1, 1))
                    continue
                # in_sel = self.config_reg(name=f"read_{i}_to_write_{j}_sel", width=num_write_ctrs_bw)
                in_sel = self.config_reg(name=f"read_{i}_to_write_{j}_sel_in", width=num_rd_dims_bw)
                # self.in_sels_rd_to_wr.append(in_sel)
                write_sg = self.writes[j]
                write_iters = self._writer_iterators[j]
                write_extents = self._writer_extents[j]
                write_finisheds = self._writer_finished[j]
                # read_iters = read_sg.get_iterator_intf()['iterators']
                write_width = write_iters.width

                num_wr_dims = write_sg.get_dimensionality()
                nsel_w = self._num_sel_levels(num_wr_dims)
                num_read_ctrs_bw = max(1, kts.clog2(nsel_w))
                # out_sel = kts.const(0, num_read_ctrs_bw)
                out_sel = self.config_reg(name=f"read_{i}_to_write_{j}_sel_out", width=num_read_ctrs_bw)

                lf_comp_block = self._lf_block(LFComparisonOperator.LT, read_width, write_width)
                self.lfcs[f"rd_to_wr_{i}_{j}"] = lf_comp_block
                self.levels_map[f"rd_to_wr_{i}_{j}"] = (nsel_r, nsel_w)
                # Now have lf block, need to instantiate it, then wire up a mux based on the config
                lf_comp_block.gen_hardware()
                self.add_child(f"lfcompblock_r_{i}_w_{j}", lf_comp_block,
                               clk=self._clk, rst_n=self._rst_n,
                               clk_en=self._clk_en, flush=self._flush)
                lf_comp_block_intfs = lf_comp_block.get_interfaces()
                # the ith writer block, compared against jth reader
                self.wire(self._reader_comparisons[i][j], lf_comp_block_intfs['comparison'])
                # Finally, we need to mux in the values based on config_regs

                # To handle overflow, we need to also consider the counter value above - if they are different, we need to modulate the counter
                p1_counter_r = self._outer(f"read_{i}_to_write_{j}_ctr_in_p1", f"lfcompblock_r_{i}_w_{j}_input_mux_ctr_p1",
                                           in_sel, read_iters, read_sg.get_dimensionality(), read_width, nsel_r)
                p1_counter_w = self._outer(f"read_{i}_to_write_{j}_ctr_out_p1", f"lfcompblock_r_{i}_w_{j}_output_mux_ctr_p1",
                                           out_sel, write_iters, write_sg.get_dimensionality(), write_width, nsel_w)

                # I think the input extent and output extent have to be the same (?) but not for line buffer - I think we just want to add the write
                # extent always since the write always goes over all data
                input_extent = self.var(name=f"read_{i}_write_{j}_input_extent", width=read_width)
                output_extent = self.var(name=f"read_{i}_write_{j}_output_extent", width=write_width)

                # Mux in the extents of the chosen level for overflow comparison
                inline_multiplexer(generator=self, name=f"r_{i}_w_{j}_input_extent_mux", sel=in_sel, one=input_extent,
                                   many=self._sel_levels(read_extents, nsel_r, num_read_ctrs), one_hot_sel=False)

                inline_multiplexer(generator=self, name=f"r_{i}_w_{j}_output_extent_mux", sel=out_sel, one=output_extent,
                                   many=self._sel_levels(write_extents, nsel_w, num_wr_dims), one_hot_sel=False)

                # Now mux in the proper counter and add to it the write extent if the outer layer is different
                outer_level_different = self.var(f"r_{i}_w_{j}_outer_level_different", 1)
                self.wire(outer_level_different, p1_counter_w != p1_counter_r)

                chosen_write_iter = self.var(f"read_{i}_write_{j}_chosen_write_iter", width=write_width)
                # chosen_read_iter = self.var(f"read_{i}_write_{j}_chosen_read_iter", width=write_width)

                # mux and attach to lf comp block
                inline_multiplexer(generator=self, name=f"lfcompblock_r_{i}_w_{j}_input_mux_ctr", sel=in_sel, one=lf_comp_block_intfs['in_counter'],
                                   many=self._sel_levels(read_iters, nsel_r, num_read_ctrs), one_hot_sel=False)

                self.wire(lf_comp_block_intfs['in_finished'], read_finisheds)
                self.wire(lf_comp_block_intfs['out_finished'], write_finisheds)

                # self.out_sels_rd_to_wr.append(out_sel)
                self.sels_map[f"rd_to_wr_{i}_{j}"] = (in_sel, out_sel)
                inline_multiplexer(generator=self, name=f"lfcompblock_r_{i}_w_{j}_output_mux_ctr", sel=out_sel, one=chosen_write_iter,
                                   many=self._sel_levels(write_iters, nsel_w, num_wr_dims), one_hot_sel=False)

                self.wire(lf_comp_block_intfs['out_counter'], kts.ternary(outer_level_different, chosen_write_iter + input_extent, chosen_write_iter))

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def gen_bitstream(self, constraints):
        self.clear_configuration()
        # Every constraint in constraints is between a port,port,comparator,offset
        # We make all input ports come before outputs ports so we can simply define the space
        # by an integer (self.rw_div)
        # print(constraints)
        for constraint in constraints:
            p1, p1_ctr, p2, p2_ctr, comparator, scalar = constraint
            p1reg, p2reg = self.get_mux_sel_reg_from_indexes(p1, p2)
            barrier = self.barrier_bit and self._is_barrier(comparator, scalar)
            if barrier:
                # levels and scalar are irrelevant: the barrier bit holds the comparison at 0
                p1_ctr = p2_ctr = 0
            elif self.max_level is not None:
                self._check_levels(constraint)
            # p1 and p2 go to local, comparator and scalar go below
            self.configure(p1reg, p1_ctr)
            self.configure(p2reg, p2_ctr)
            # Now configure the lfcompare blocks
            lfc_: LFCompBlock = self.get_lfc_from_indexes(p1, p2)
            # TODO: make this automatic - when configuring a local reg it should be easy to grab it,
            # otherwise automatically add the offsets and values
            if barrier:
                lfc_bs = lfc_.gen_bitstream(comparator=comparator, scalar=0, barrier=True)
            else:
                lfc_bs = lfc_.gen_bitstream(comparator=comparator, scalar=scalar)
            lfc_bs = self._add_base_to_cfg_space(lfc_bs, self.child_cfg_bases[lfc_])
            self._add_configuration_manual(config=lfc_bs)

        return self.get_configuration()

    @staticmethod
    def _is_barrier(comparator, scalar):
        """A constraint no in-range counter pair satisfies (exact while
        iterators are <= 14 bits): LT with scalar >= RV_BARRIER_SCALAR, or the
        same constraint written as GT (scalar <= -RV_BARRIER_SCALAR)."""
        if comparator == LFComparisonOperator.LT.value:
            return scalar >= RV_BARRIER_SCALAR
        if comparator == LFComparisonOperator.GT.value:
            return scalar <= -RV_BARRIER_SCALAR
        return False

    def _check_levels(self, constraint):
        p1, p1_ctr, p2, p2_ctr = constraint[:4]
        if p1 < self.rw_div:
            key = f"wr_to_rd_{p1}_{p2 - self.rw_div}"
        else:
            key = f"rd_to_wr_{p1 - self.rw_div}_{p2}"
        n1, n2 = self.levels_map[key]
        if not (0 <= p1_ctr < n1 and 0 <= p2_ctr < n2):
            raise ValueError(f"RV comparison network ({key}): constraint {tuple(constraint)} selects level "
                             f"({p1_ctr}, {p2_ctr}); this network's level select covers 0..{n1 - 1} / 0..{n2 - 1} "
                             f"(max_level {self.max_level})")
