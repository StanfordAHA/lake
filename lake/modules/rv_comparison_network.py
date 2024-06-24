import kratos as kts
from lake.utils.spec_enum import Direction
from lake.spec.schedule_generator import ScheduleGenerator
from lake.modules.lf_comp_block import LFCompBlock
from lake.utils.util import inline_multiplexer


class RVComparisonNetwork(kts.Generator):

    def __init__(self, name: str, debug: bool = False, is_clone: bool = False, internal_generator=None,
                 writes=None, reads=None):
        super().__init__(name, debug, is_clone, internal_generator)
        self.writes = []
        self.reads = []
        if writes is not None:
            self.writes.extend(writes)
        if reads is not None:
            self.reads.extend(reads)

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

    def gen_hardware(self):

        # Build out the interfaces first - each writer will get a number of comparisons and so will reads
        self._writer_comparisons = [self.output(f"write_{i}_comparisons", len(self.reads)) for i in range(len(self.writes))]
        self._writer_iterators = []
        for i in range(len(self.writes)):
            writer_sg = self.writes[i]
            writer_sg_iter_intf = writer_sg.get_iterator_intf()
            iterators_ = writer_sg_iter_intf['iterators']
            comparisons_ = writer_sg_iter_intf['comparisons']
            dim_ = writer_sg.get_dimensionality()
            iter_width = iterators_.width
            self._writer_iterators.append(self.input(f"write_{i}_iterators", iter_width, size=dim_, packed=True, explicit_array=True))
            self.connections.append((iterators_, self._writer_iterators[i]))
            self.connections.append((comparisons_, self._writer_comparisons[i]))

        self._reader_comparisons = [self.output(f"read_{i}_comparisons", len(self.reads)) for i in range(len(self.writes))]
        self._reader_iterators = []
        for i in range(len(self.reads)):
            reader_sg = self.reads[i]
            reader_sg_iter_intf = reader_sg.get_iterator_intf()
            iterators_ = reader_sg_iter_intf['iterators']
            comparisons_ = reader_sg_iter_intf['comparisons']
            dim_ = reader_sg.get_dimensionality()
            iter_width = iterators_.width
            self._reader_iterators.append(self.input(f"read_{i}_iterators", iter_width, size=dim_, packed=True, explicit_array=True))
            self.connections.append((iterators_, self._reader_iterators[i]))
            self.connections.append((comparisons_, self._reader_comparisons[i]))

        # Now, for every writer, we will build a LF block for every reader, same for the other way around
        for i in range(len(self.writes)):
            write_sg = self.writes[i]
            write_iters = self._writer_iterators[i]
            write_width = write_iters.width
            num_write_ctrs = write_sg.get_dimensionality()
            num_write_ctrs_bw = max(1, kts.clog2(num_write_ctrs))
            in_sel = kts.const(0, num_write_ctrs_bw)

            for j in range(len(self.reads)):
                read_sg = self.reads[i]
                read_iters = self._reader_iterators[j]
                # read_iters = read_sg.get_iterator_intf()['iterators']
                read_width = read_iters.width
                lf_comp_block = LFCompBlock(name='lf_comp_block', in_width=write_width, out_width=read_width)
                # Now have lf block, need to instantiate it, then wire up a mux based on the config
                lf_comp_block.gen_hardware()
                self.add_child(f"lfcompblock_w_{i}_r_{j}", lf_comp_block)
                lf_comp_block_intfs = lf_comp_block.get_interfaces()
                # the ith writer block, compared against jth reader
                self.wire(self._writer_comparisons[i][j], lf_comp_block_intfs['comparison'])
                # Finally, we need to mux in the values based on config_regs
                # mux and attach to lf comp block
                inline_multiplexer(generator=self, name=f"lfcompblock_w_{i}_r_{j}_input_mux_ctr", sel=in_sel, one=lf_comp_block_intfs['in_counter'], many=write_iters,
                                   one_hot_sel=False)

                num_read_ctrs = read_sg.get_dimensionality()
                num_read_ctrs_bw = max(1, kts.clog2(num_read_ctrs))
                out_sel = kts.const(0, num_read_ctrs_bw)
                inline_multiplexer(generator=self, name=f"lfcompblock_w_{i}_r_{j}_output_mux_ctr", sel=out_sel, one=lf_comp_block_intfs['out_counter'], many=read_iters,
                                   one_hot_sel=False)

        # Now - same for the other way around
        for i in range(len(self.reads)):
            read_sg = self.reads[i]
            read_iters = self._reader_iterators[i]
            read_width = read_iters.width
            num_read_ctrs = read_sg.get_dimensionality()
            num_read_ctrs_bw = max(1, kts.clog2(num_write_ctrs))
            in_sel = kts.const(0, num_read_ctrs_bw)
            for j in range(len(self.writes)):
                write_sg = self.writes[i]
                write_iters = self._writer_iterators[j]
                # read_iters = read_sg.get_iterator_intf()['iterators']
                write_width = write_iters.width
                lf_comp_block = LFCompBlock(name='lf_comp_block', in_width=read_width, out_width=write_width)
                # Now have lf block, need to instantiate it, then wire up a mux based on the config
                lf_comp_block.gen_hardware()
                self.add_child(f"lfcompblock_r_{i}_w_{j}", lf_comp_block)
                lf_comp_block_intfs = lf_comp_block.get_interfaces()
                # the ith writer block, compared against jth reader
                self.wire(self._reader_comparisons[i][j], lf_comp_block_intfs['comparison'])
                # Finally, we need to mux in the values based on config_regs

                # mux and attach to lf comp block
                inline_multiplexer(generator=self, name=f"lfcompblock_r_{i}_w_{j}_input_mux_ctr", sel=in_sel, one=lf_comp_block_intfs['in_counter'], many=read_iters,
                                   one_hot_sel=False)

                num_read_ctrs = write_sg.get_dimensionality()
                num_read_ctrs_bw = max(1, kts.clog2(num_read_ctrs))
                out_sel = kts.const(0, num_read_ctrs_bw)
                inline_multiplexer(generator=self, name=f"lfcompblock_r_{i}_w_{j}_output_mux_ctr", sel=out_sel, one=lf_comp_block_intfs['out_counter'], many=write_iters,
                                   one_hot_sel=False)
