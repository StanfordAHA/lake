import kratos as kts
from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.attributes.formal_attr import FormalAttr
from lake.top.memory_controller import MemoryController
from lake.modules.reg_cr import Reg
from lake.modules.onyx_pe import OnyxPE
from lake.attributes.hybrid_port_attr import HybridPortAddr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.passes.passes import lift_config_reg


class ReducePECluster(MemoryController):
    def __init__(self,
                 data_width=16,
                 add_dispatcher=False,
                 dispatcher_size=2,
                 fifo_depth=8,
                 defer_fifo=True,
                 add_flush=False,
                 perf_debug=True,
                 pe_prefix="PG_",
                 do_lift_config=False):
        super().__init__("reduce_pe_cluster", debug=True)

        self.data_width = data_width
        self.add_flush = add_flush
        self.defer_fifo = defer_fifo
        self.perf_debug = perf_debug
        self.fifo_depth = fifo_depth

        self.add_dispatcher = add_dispatcher
        self.dispatcher_size = dispatcher_size
        self.pe_prefix = pe_prefix
        self.do_lift_config = do_lift_config

        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        # Select signal for internal/external PE input
        self._pe_in_external = self.input("pe_in_external", 1)
        self._pe_in_external.add_attribute(ConfigRegAttr("This logic selects between the internal/external data I/O for the PE"))

        # Clock gating logic
        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # The I/O interface for the reducer
        self._reduce_data_in = self.input("reduce_data_in", self.data_width + 1, packed=True)
        self._reduce_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._reduce_valid_in = self.input("reduce_data_in_valid", 1)
        self._reduce_valid_in.add_attribute(ControlSignalAttr(is_control=True))
        self._reduce_ready_out = self.output("reduce_data_in_ready", 1)
        self._reduce_ready_out.add_attribute(ControlSignalAttr(is_control=False))
        self._reduce_data_out = self.output("reduce_data_out", self.data_width + 1, packed=True)
        self._reduce_data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._reduce_ready_in = self.input("reduce_data_out_ready", 1)
        self._reduce_ready_in.add_attribute(ControlSignalAttr(is_control=True))
        self._reduce_valid_out = self.output("reduce_data_out_valid", 1)
        self._reduce_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # Data interface signals between the reduce and the PE
        # PE finishes within a single cycle, no need for valid ready interface between reduce and pe
        self.reduce_data_to_pe = []
        for i in range(2):
            tmp_reduce_data_to_pe = self.var(f"reduce_data_to_pe{i}", self.data_width + 1, packed=True)
            self.reduce_data_to_pe.append(tmp_reduce_data_to_pe)

        self._pe_data_to_reduce = self.var("pe_data_to_reduce", self.data_width + 1, packed=True)

        # Instantiate the reduce module here
        self.reduce = Reg(data_width=16,
                          fifo_depth=self.fifo_depth,
                          defer_fifos=self.defer_fifo,
                          perf_debug=perf_debug)

        self.add_child("reduce",
                       self.reduce,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       data_in=self._reduce_data_in,
                       data_in_valid=self._reduce_valid_in,
                       data_in_ready=self._reduce_ready_out,
                       data_out=self._reduce_data_out,
                       data_out_ready=self._reduce_ready_in,
                       data_out_valid=self._reduce_valid_out,
                       data_to_pe0=self.reduce_data_to_pe[0],
                       data_to_pe1=self.reduce_data_to_pe[1],
                       data_from_pe=self._pe_data_to_reduce)

        self.pe_data_in = []
        # Data and ready valid IO for the PE from outside the cluster
        self.ext_pe_data_in = []
        self.ext_pe_data_in_valid_in = []
        self.ext_pe_data_in_ready_out = []
        self.pe_bit_in = []
        for i in range(3):
            tmp_pe_data_in = self.var(f"pe_data{i}", self.data_width + 1, packed=True)
            # external data input
            tmp_ext_pe_data_in_valid_in = self.input(f"data{i}_valid", 1)
            tmp_ext_pe_data_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))
            tmp_ext_pe_data_in_ready_out = self.output(f"data{i}_ready", 1)
            tmp_ext_pe_data_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

            self.pe_data_in.append(tmp_pe_data_in)
            self.ext_pe_data_in_valid_in.append(tmp_ext_pe_data_in_valid_in)
            self.ext_pe_data_in_ready_out.append(tmp_ext_pe_data_in_ready_out)

            tmp_ext_pe_data_in = self.input(f"data{i}", self.data_width + 1, packed=True)
            tmp_ext_pe_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            # Mark as hybrid port to allow bypassing at the core combiner level
            tmp_ext_pe_data_in.add_attribute(HybridPortAddr())

            self.ext_pe_data_in.append(tmp_ext_pe_data_in)

        # The 1-bit data I/O
        for i in range(3):
            tmp_pe_data_in = self.input(f"bit{i}", 1)
            tmp_pe_data_in.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

            self.pe_bit_in.append(tmp_pe_data_in)

        # PE results output to other primitives external to this cluster
        self._pe_data_out = self.output("res", self.data_width + 1, packed=True)
        self._pe_data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._pe_data_out.add_attribute(HybridPortAddr())

        self._pe_valid_out = self.output("res_valid", 1)
        self._pe_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._pe_ready_in = self.input("res_ready", 1)
        self._pe_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._pe_data_out_p = self.output("res_p", 1)
        self._pe_data_out_p.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        # Instantiate the PE here
        self.onyxpe = OnyxPE(data_width=16,
                            fifo_depth=self.fifo_depth,
                            defer_fifos=self.defer_fifo,
                            ext_pe_prefix=self.pe_prefix,
                            pe_ro=True,
                            do_config_lift=False,
                            perf_debug=perf_debug)

        self.add_child("pe",
                       self.onyxpe,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       data0=self.pe_data_in[0],
                       data0_valid=self.ext_pe_data_in_valid_in[0],
                       data0_ready=self.ext_pe_data_in_ready_out[0],
                       data1=self.pe_data_in[1],
                       data1_valid=self.ext_pe_data_in_valid_in[1],
                       data1_ready=self.ext_pe_data_in_ready_out[1],
                       data2=self.pe_data_in[2],
                       data2_valid=self.ext_pe_data_in_valid_in[2],
                       data2_ready=self.ext_pe_data_in_ready_out[2],
                       bit0=self.pe_bit_in[0],
                       bit1=self.pe_bit_in[1],
                       bit2=self.pe_bit_in[2],
                       res=self._pe_data_out,
                       res_valid=self._pe_valid_out,
                       res_ready=self._pe_ready_in,
                       res_p=self._pe_data_out_p)

        # Select between the internal/external signal connection for the PE
        self.wire(self.pe_data_in[0], kts.ternary(self._pe_in_external,
                                                   self.ext_pe_data_in[0],
                                                   self.reduce_data_to_pe[0]))
        self.wire(self.pe_data_in[1], kts.ternary(self._pe_in_external,
                                                   self.ext_pe_data_in[1],
                                                   self.reduce_data_to_pe[1]))
        # Reduce does not use the third input of the pe, just wire it to the external inputs
        self.wire(self.pe_data_in[2], self.ext_pe_data_in[2])

        # Wire the output of the pe also to the reduce
        self.wire(self._pe_data_to_reduce, self.onyxpe.ports.res)

        # clk enable logic
        kts.passes.auto_insert_clock_enable(self.internal_generator)
        clk_en_port = self.internal_generator.get_port("clk_en")
        clk_en_port.add_attribute(ControlSignalAttr(False))

        flush_port = None
        if self.add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

        # Lift the configuration register of the internal modules so they are
        # visible at the cluster level
        if self.do_lift_config:
            lift_config_reg(self.internal_generator)

    def get_bitstream(self, config_kwargs):
        assert 'pe_only' in config_kwargs
        assert 'pe_in_external' in config_kwargs

        pe_only = config_kwargs['pe_only']
        pe_in_external = config_kwargs['pe_in_external']

        config = []
        # configure the specific module according to the keyword specified
        if pe_only:
            sub_config = self.onyxpe.get_bitstream(config_kwargs=config_kwargs)
            for config_tuple in sub_config:
                config_name, config_value = config_tuple
                config += [(f"{self.onyxpe.instance_name}_{config_name}", config_value)]
        else:
            sub_config = self.reduce.get_bitstream(config_kwargs=config_kwargs)
            for config_tuple in sub_config:
                config_name, config_value = config_tuple
                config += [(f"{self.reduce.instance_name}_{config_name}", config_value)]
            sub_config = self.onyxpe.get_bitstream(config_kwargs=config_kwargs)
            for config_tuple in sub_config:
                config_name, config_value = config_tuple
                config += [(f"{self.onyxpe.instance_name}_{config_name}", config_value)]

        config += [("tile_en", 1)]
        config += [("pe_in_external", pe_in_external)]

        return config

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        # FIXME: Override the config mode str of underlying pe since in dense mode, the mapping/routing
        # logic look for this keyword to perfrom mapping and routing to/from the pe
        return "alu"


if __name__ == "__main__":
    reduce_pe_cluster_dut = ReducePECluster(data_width=16,
                                            fifo_depth=2,
                                            defer_fifo=False,
                                            add_flush=False,
                                            perf_debug=False,
                                            do_lift_config=True)
    kts.verilog(reduce_pe_cluster_dut,
                filename="reduce_pe_cluster.sv",
                optimize_if=False)
