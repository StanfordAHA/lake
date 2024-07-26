import kratos as kts
from lake.utils.spec_enum import Direction
from lake.spec.schedule_generator import ScheduleGenerator
from lake.modules.lf_comp_block import LFCompBlock, LFComparisonOperator
from lake.utils.util import inline_multiplexer
from lake.spec.component import Component


class RVComparisonNetwork(Component):

    def __init__(self, name: str, debug: bool = False, is_clone: bool = False, internal_generator=None,
                 writes=None, reads=None):
        super().__init__(name=name)
        # super().__init__(name, debug, is_clone, internal_generator)
        self.writes = []
        self.reads = []
        if writes is not None:
            self.writes.extend(writes)
        if reads is not None:
            self.reads.extend(reads)
        self.rw_div = None

        self.lfcs = {}

        # self.in_sels_wr_to_rd = []
        # self.out_sels_wr_to_rd = []
        # self.in_sels_rd_to_wr = []
        # self.out_sels_rd_to_wr = []

        self.sels_map = {}

        self.connections = []

    def add_reader_writer(self, direction: Direction, sg: ScheduleGenerator):
        # Based on the direction, register this with either self.writes or self.reads
        if direction == Direction.IN:
            self.writes.append(sg)
        elif direction == Direction.OUT:
            self.reads.append(sg)
        else:
            raise NotImplementedError

    def get_connections(self):
        return self.connections

    def get_port_from_index(self, p_idx: int):

        assert self.rw_div is not None
        if p_idx < self.rw_div:
            return self.writes[p_idx]
        else:
            return self.reads[p_idx - self.rw_div]

    def get_mux_sel_reg_from_indexes(self, p1, p2):

        print(p1)
        print(p2)

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

        return self.sels_map[f"{rd_to_wr}_{adj_p1_idx}_{adj_p2_idx}"]
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

        return self.lfcs[f"{rd_to_wr}_{adj_p1_idx}_{adj_p2_idx}"]

    def gen_hardware(self):

        # Let's assume that the ports involved here are fixed at this point...
        # So we can combine the writes and reads into a 'single' list
        self.rw_div = len(self.writes)
        # Build out the interfaces first - each writer will get a number of comparisons and so will reads
        self._writer_comparisons = [self.output(f"write_{i}_comparisons", len(self.reads)) for i in range(len(self.writes))]
        self._writer_iterators = []
        self._writer_extents = []
        for i in range(len(self.writes)):
            writer_sg = self.writes[i]
            writer_sg_iter_intf = writer_sg.get_iterator_intf()
            iterators_ = writer_sg_iter_intf['iterators']
            comparisons_ = writer_sg_iter_intf['comparisons']
            extents_ = writer_sg_iter_intf['extents']
            dim_ = writer_sg.get_dimensionality()
            iter_width = iterators_.width
            self._writer_iterators.append(self.input(f"write_{i}_iterators", iter_width, size=dim_, packed=True, explicit_array=True))
            self._writer_extents.append(self.input(f"write_{i}_extents", iter_width, size=dim_, packed=True, explicit_array=True))
            self.connections.append((iterators_, self._writer_iterators[i]))
            self.connections.append((comparisons_, self._writer_comparisons[i]))
            self.connections.append((extents_, self._writer_extents[i]))

        self._reader_comparisons = [self.output(f"read_{i}_comparisons", len(self.reads)) for i in range(len(self.writes))]
        self._reader_iterators = []
        self._reader_extents = []
        for i in range(len(self.reads)):
            reader_sg = self.reads[i]
            reader_sg_iter_intf = reader_sg.get_iterator_intf()
            iterators_ = reader_sg_iter_intf['iterators']
            comparisons_ = reader_sg_iter_intf['comparisons']
            extents_ = reader_sg_iter_intf['extents']
            dim_ = reader_sg.get_dimensionality()
            iter_width = iterators_.width
            self._reader_iterators.append(self.input(f"read_{i}_iterators", iter_width, size=dim_, packed=True, explicit_array=True))
            self._reader_extents.append(self.input(f"read_{i}_extents", iter_width, size=dim_, packed=True, explicit_array=True))
            self.connections.append((iterators_, self._reader_iterators[i]))
            self.connections.append((comparisons_, self._reader_comparisons[i]))
            self.connections.append((extents_, self._reader_extents[i]))

        # Now, for every writer, we will build a LF block for every reader, same for the other way around (W->R)
        for i in range(len(self.writes)):
            write_sg = self.writes[i]
            write_iters = self._writer_iterators[i]
            write_extents = self._writer_extents[i]
            write_width = write_iters.width
            num_write_ctrs = write_sg.get_dimensionality()
            num_write_ctrs_bw = max(1, kts.clog2(num_write_ctrs))
            # Config register!
            # in_sel = self.config_reg(name=f"write_{i}_to_read_sel", width=num_write_ctrs_bw)
            # in_sel = kts.const(0, num_write_ctrs_bw)

            for j in range(len(self.reads)):
                in_sel = self.config_reg(name=f"write_{i}_to_read_{j}_sel_in", width=num_write_ctrs_bw)
                # self.in_sels_wr_to_rd.append(in_sel)
                read_sg = self.reads[j]
                read_iters = self._reader_iterators[j]
                read_extents = self._reader_extents[j]
                # read_iters = read_sg.get_iterator_intf()['iterators']
                read_width = read_iters.width
                num_read_ctrs = read_sg.get_dimensionality()
                num_read_ctrs_bw = max(1, kts.clog2(num_read_ctrs))

                out_sel = self.config_reg(name=f"write_{i}_to_read_{j}_sel_out", width=num_read_ctrs_bw)

                # Instantiate LF block
                lf_comp_block = LFCompBlock(name='lf_comp_block', in_width=write_width, out_width=read_width)
                self.lfcs[f"wr_to_rd_{i}_{j}"] = lf_comp_block
                # Now have lf block, need to instantiate it, then wire up a mux based on the config
                lf_comp_block.gen_hardware()
                self.add_child(f"lfcompblock_w_{i}_r_{j}", lf_comp_block,
                               clk=self._clk, rst_n=self._rst_n)
                lf_comp_block_intfs = lf_comp_block.get_interfaces()
                # the ith writer block, compared against jth reader
                self.wire(self._writer_comparisons[i][j], lf_comp_block_intfs['comparison'])

                in_sel_p1 = in_sel + 1
                p1_counter_w = self.var(name=f"write_{i}_to_read_{j}_ctr_in_p1", width=write_width)
                # To handle overflow, we need to also consider the counter value above - if they are different, we need to modulate the counter
                inline_multiplexer(generator=self, name=f"lfcompblock_w_{i}_r_{j}_input_mux_ctr_p1", sel=in_sel_p1, one=p1_counter_w, many=write_iters,
                                   one_hot_sel=False)

                out_sel_p1 = out_sel + 1
                p1_counter_r = self.var(name=f"write_{i}_to_read_{j}_ctr_out_p1", width=read_width)
                # To handle overflow, we need to also consider the counter value above - if they are different, we need to modulate the counter
                inline_multiplexer(generator=self, name=f"lfcompblock_w_{i}_r_{j}_output_mux_ctr_p1", sel=out_sel_p1, one=p1_counter_r, many=read_iters,
                                   one_hot_sel=False)

                # I think the input extent and output extent have to be the same (?) but not for line buffer - I think we just want to add the write
                # extent always since the write always goes over all data
                input_extent = self.var(name=f"write_{i}_read_{j}_input_extent", width=write_width)
                output_extent = self.var(name=f"write_{i}_read_{j}_output_extent", width=read_width)

                # Mux in the extents of the chosen level for overflow comparison
                inline_multiplexer(generator=self, name=f"w_{i}_r_{j}_input_extent_mux", sel=in_sel, one=input_extent, many=write_extents,
                                   one_hot_sel=False)

                inline_multiplexer(generator=self, name=f"w_{i}_r_{j}_output_extent_mux", sel=out_sel, one=output_extent, many=read_extents,
                                   one_hot_sel=False)

                # Now mux in the proper counter and add to it the write extent if the outer layer is different
                outer_level_different = self.var(f"w_{i}_r_{j}_outer_level_different", 1)
                self.wire(outer_level_different, p1_counter_w != p1_counter_r)

                chosen_write_iter = self.var(f"write_{i}_read_{j}_chosen_write_iter", width=write_width)
                # chosen_read_iter = self.var(f"write_{i}_read_{j}_chosen_read_iter", width=read_width)

                # Finally, we need to mux in the values based on config_regs
                # mux and attach to lf comp block
                inline_multiplexer(generator=self, name=f"lfcompblock_w_{i}_r_{j}_input_mux_ctr", sel=in_sel, one=chosen_write_iter, many=write_iters,
                                   one_hot_sel=False)

                self.wire(lf_comp_block_intfs['in_counter'], kts.ternary(outer_level_different, chosen_write_iter + input_extent, chosen_write_iter))

                # out_sel = kts.const(0, num_read_ctrs_bw)
                # self.out_sels_wr_to_rd.append(out_sel)
                self.sels_map[f"wr_to_rd_{i}_{j}"] = (in_sel, out_sel)
                # out_sel = kts.const(0, num_read_ctrs_bw)
                inline_multiplexer(generator=self, name=f"lfcompblock_w_{i}_r_{j}_output_mux_ctr", sel=out_sel, one=lf_comp_block_intfs['out_counter'], many=read_iters,
                                   one_hot_sel=False)

        # Now - same for the other way around (R->W)
        for i in range(len(self.reads)):
            read_sg = self.reads[i]
            read_iters = self._reader_iterators[i]
            read_width = read_iters.width
            read_extents = self._reader_extents[i]
            num_read_ctrs = read_sg.get_dimensionality()
            num_read_ctrs_bw = max(1, kts.clog2(num_write_ctrs))
            # in_sel = kts.const(0, num_read_ctrs_bw)
            # in_sel = self.config_reg(name=f"read_{i}_to_write_sel", width=num_write_ctrs_bw)
            for j in range(len(self.writes)):
                # in_sel = self.config_reg(name=f"read_{i}_to_write_{j}_sel", width=num_write_ctrs_bw)
                in_sel = self.config_reg(name=f"read_{i}_to_write_{j}_sel_in", width=num_write_ctrs_bw)
                # self.in_sels_rd_to_wr.append(in_sel)
                write_sg = self.writes[j]
                write_iters = self._writer_iterators[j]
                write_extents = self._writer_extents[j]
                # read_iters = read_sg.get_iterator_intf()['iterators']
                write_width = write_iters.width

                num_read_ctrs = write_sg.get_dimensionality()
                num_read_ctrs_bw = max(1, kts.clog2(num_read_ctrs))
                # out_sel = kts.const(0, num_read_ctrs_bw)
                out_sel = self.config_reg(name=f"read_{i}_to_write_{j}_sel_out", width=num_read_ctrs_bw)

                lf_comp_block = LFCompBlock(name='lf_comp_block', in_width=read_width, out_width=write_width)
                self.lfcs[f"rd_to_wr_{i}_{j}"] = lf_comp_block
                # Now have lf block, need to instantiate it, then wire up a mux based on the config
                lf_comp_block.gen_hardware()
                self.add_child(f"lfcompblock_r_{i}_w_{j}", lf_comp_block,
                               clk=self._clk, rst_n=self._rst_n)
                lf_comp_block_intfs = lf_comp_block.get_interfaces()
                # the ith writer block, compared against jth reader
                self.wire(self._reader_comparisons[i][j], lf_comp_block_intfs['comparison'])
                # Finally, we need to mux in the values based on config_regs

                in_sel_p1 = in_sel + 1
                p1_counter_r = self.var(name=f"read_{i}_to_write_{j}_ctr_in_p1", width=read_width)
                # To handle overflow, we need to also consider the counter value above - if they are different, we need to modulate the counter
                inline_multiplexer(generator=self, name=f"lfcompblock_r_{i}_w_{j}_input_mux_ctr_p1", sel=in_sel_p1, one=p1_counter_r, many=read_iters,
                                   one_hot_sel=False)

                out_sel_p1 = out_sel + 1
                p1_counter_w = self.var(name=f"read_{i}_to_write_{j}_ctr_out_p1", width=write_width)
                # To handle overflow, we need to also consider the counter value above - if they are different, we need to modulate the counter
                inline_multiplexer(generator=self, name=f"lfcompblock_r_{i}_w_{j}_output_mux_ctr_p1", sel=out_sel_p1, one=p1_counter_w, many=write_iters,
                                   one_hot_sel=False)

                # I think the input extent and output extent have to be the same (?) but not for line buffer - I think we just want to add the write
                # extent always since the write always goes over all data
                input_extent = self.var(name=f"read_{i}_write_{j}_input_extent", width=read_width)
                output_extent = self.var(name=f"read_{i}_write_{j}_output_extent", width=write_width)

                # Mux in the extents of the chosen level for overflow comparison
                inline_multiplexer(generator=self, name=f"r_{i}_w_{j}_input_extent_mux", sel=in_sel, one=input_extent, many=read_extents,
                                   one_hot_sel=False)

                inline_multiplexer(generator=self, name=f"r_{i}_w_{j}_output_extent_mux", sel=out_sel, one=output_extent, many=write_extents,
                                   one_hot_sel=False)

                # Now mux in the proper counter and add to it the write extent if the outer layer is different
                outer_level_different = self.var(f"r_{i}_w_{j}_outer_level_different", 1)
                self.wire(outer_level_different, p1_counter_w != p1_counter_r)

                chosen_write_iter = self.var(f"read_{i}_write_{j}_chosen_write_iter", width=write_width)
                # chosen_read_iter = self.var(f"read_{i}_write_{j}_chosen_read_iter", width=write_width)

                # mux and attach to lf comp block
                inline_multiplexer(generator=self, name=f"lfcompblock_r_{i}_w_{j}_input_mux_ctr", sel=in_sel, one=lf_comp_block_intfs['in_counter'], many=read_iters,
                                   one_hot_sel=False)

                # self.out_sels_rd_to_wr.append(out_sel)
                self.sels_map[f"rd_to_wr_{i}_{j}"] = (in_sel, out_sel)
                inline_multiplexer(generator=self, name=f"lfcompblock_r_{i}_w_{j}_output_mux_ctr", sel=out_sel, one=chosen_write_iter, many=write_iters,
                                   one_hot_sel=False)

                self.wire(lf_comp_block_intfs['out_counter'], kts.ternary(outer_level_different, chosen_write_iter + input_extent, chosen_write_iter))

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def gen_bitstream(self, constraints):
        # Every constraint in constraints is between a port,port,comparator,offset
        # We make all input ports come before outputs ports so we can simply define the space
        # by an integer (self.rw_div)
        print(constraints)
        for constraint in constraints:
            p1, p1_ctr, p2, p2_ctr, comparator, scalar = constraint
            p1reg, p2reg = self.get_mux_sel_reg_from_indexes(p1, p2)
            # p1 and p2 go to local, comparator and scalar go below
            self.configure(p1reg, p1_ctr)
            self.configure(p2reg, p2_ctr)
            # Now configure the lfcompare blocks
            lfc_: LFCompBlock = self.get_lfc_from_indexes(p1, p2)
            # TODO: make this automatic - when configuring a local reg it should be easy to grab it,
            # otherwise automatically add the offsets and values
            lfc_bs = lfc_.gen_bitstream(comparator=comparator, scalar=scalar)
            lfc_bs = self._add_base_to_cfg_space(lfc_bs, self.child_cfg_bases[lfc_])
            self._add_configuration_manual(config=lfc_bs)

        return self.get_configuration()
