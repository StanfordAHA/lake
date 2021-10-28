from lake.top.memory_controller import MemoryController
from lake.top.memory_interface import MemoryPort, MemoryPortType
from lake.utils.parse_clkwork_config import configure_controller, extract_controller, extract_controller_json, map_controller
from lake.utils.util import get_priority_encode
from kratos import *
from lake.modules.passthru import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
import os
import kratos as kts


class StrgUBThin(MemoryController):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=16,
                 mem_depth=512,
                 input_addr_iterator_support=6,
                 input_sched_iterator_support=6,
                 output_addr_iterator_support=6,
                 output_sched_iterator_support=6,
                 interconnect_input_ports=1,  # Connection to int
                 interconnect_output_ports=1,
                 config_width=16,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=True,
                 gen_addr=True):

        super().__init__("strg_ub_thin", debug=True)

        assert mem_width == data_width, f"This module should only be used when the fetch width is 1!"

        self.ctrl_in = "in2regfile"
        self.ctrl_out = "regfile2out"

        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_depth = mem_depth
        self.config_width = config_width
        self.data_width = data_width
        self.input_addr_iterator_support = input_addr_iterator_support
        self.input_sched_iterator_support = input_sched_iterator_support
        self.output_addr_iterator_support = output_addr_iterator_support
        self.output_sched_iterator_support = output_sched_iterator_support
        self.rw_same_cycle = rw_same_cycle
        self.read_delay = read_delay
        self.gen_addr = gen_addr
        self.default_iterator_support = 6
        self.default_config_width = 16
        # generation parameters
        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self.base_ports = [[None]]

        self._data_in = self.input("data_in", self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        # outputs
        self._data_out = self.output("data_out", self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        self._data_to_sram = self.output("data_to_strg", self.data_width,
                                         size=self.fetch_width,
                                         packed=True)
        self._data_from_sram = self.input("data_from_strg", self.data_width,
                                          size=self.fetch_width,
                                          packed=True)
        # Early out in case...
        if self.gen_addr is False:
            # Pass through write enable, addr data and
            # read enable, addr data
            self._read = self.input("ren_in", 1)
            self._write = self.input("wen_in", 1)
            self._write_addr = self.input("write_addr", self.config_width)
            self._read_addr = self.input("read_addr", self.config_width)
            # self._cen_to_sram = self.output("cen_to_strg", 1, packed=True)
            self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
            self._ren_to_sram = self.output("ren_to_strg", 1, packed=True)
            self._wr_addr_to_sram = self.output("wr_addr_out", clog2(self.mem_depth), packed=True)
            self._rd_addr_to_sram = self.output("rd_addr_out", clog2(self.mem_depth), packed=True)
            # self._accessor_output = self.output("accessor_output", self.interconnect_output_ports)
            # self.wire(self._accessor_output, self._read)
            # self.wire(self._cen_to_sram, self._write | self._read)
            self.wire(self._wen_to_sram, self._write)
            self.wire(self._ren_to_sram, self._read)
            self.wire(self._data_out, self._data_from_sram)
            self.wire(self._data_in, self._data_to_sram)
            self.wire(self._wr_addr_to_sram, self._write_addr[clog2(self.mem_depth) - 1, 0])
            self.wire(self._rd_addr_to_sram, self._read_addr[clog2(self.mem_depth) - 1, 0])

            if self.rw_same_cycle:
                self.base_ports = [[None, None]]
                tmp0_rdaddr = self.output("tmp0_rdaddr", width=self._rd_addr_to_sram.width)
                tmp0_rden = self.output("tmp0_rden", width=self._ren_to_sram.width)
                self.wire(tmp0_rdaddr, kts.const(0, width=self._rd_addr_to_sram.width))
                self.wire(tmp0_rden, kts.const(0, width=self._ren_to_sram.width))
                rw_port = MemoryPort(MemoryPortType.READWRITE)
                rw_port_intf = rw_port.get_port_interface()
                rw_port_intf['data_in'] = self._data_to_sram
                rw_port_intf['data_out'] = None
                rw_port_intf['write_addr'] = self._wr_addr_to_sram
                rw_port_intf['write_enable'] = self._wen_to_sram
                rw_port_intf['read_addr'] = tmp0_rdaddr
                rw_port_intf['read_enable'] = tmp0_rden
                rw_port.annotate_port_signals()
                self.base_ports[0][0] = rw_port
                # Populate second port as just R
                r_port = MemoryPort(MemoryPortType.READ)
                r_port_intf = r_port.get_port_interface()
                r_port_intf['data_out'] = self._data_from_sram
                r_port_intf['read_addr'] = self._rd_addr_to_sram
                r_port_intf['read_enable'] = self._ren_to_sram
                r_port.annotate_port_signals()
                self.base_ports[0][1] = r_port
            else:
                rw_port = MemoryPort(MemoryPortType.READWRITE)
                rw_port_intf = rw_port.get_port_interface()
                rw_port_intf['data_in'] = self._data_to_sram
                rw_port_intf['data_out'] = self._data_from_sram
                rw_port_intf['write_addr'] = self._wr_addr_to_sram
                rw_port_intf['write_enable'] = self._wen_to_sram
                rw_port_intf['read_addr'] = self._rd_addr_to_sram
                rw_port_intf['read_enable'] = self._ren_to_sram
                rw_port.annotate_port_signals()
                self.base_ports[0][0] = rw_port
            return

        # Create cycle counter to share...
        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.increment_cycle_count)

        # local variables
        self._write = self.var("write", self.interconnect_input_ports)
        self._read = self.var("read", self.interconnect_output_ports)
        self._accessor_output = self.output("accessor_output", self.interconnect_output_ports)
        self.wire(self._accessor_output, self._read)

        self._valid_out = self.output("valid_out", self.interconnect_output_ports)
        if self.read_delay == 1:
            self._read_d1 = self.var("read_d1", self.interconnect_output_ports)
            self.add_code(self.delay_read)
            self.wire(self._valid_out, self._read_d1)
        else:
            self.wire(self._valid_out, self._read)

        self._write_addr = self.var("write_addr", self.config_width, size=self.interconnect_input_ports, explicit_array=True)
        self._read_addr = self.var("read_addr", self.config_width, size=self.interconnect_output_ports, explicit_array=True)
        self._addr = self.var("addr", clog2(self.mem_depth))

        # Set up addr/cycle gens for input side
        for i in range(self.interconnect_input_ports):

            FOR_LOOP_WRITE = ForLoop(iterator_support=self.default_iterator_support,
                                     config_width=self.default_config_width)

            ADDR_WRITE = AddrGen(iterator_support=self.default_iterator_support,
                                 config_width=self.default_config_width)
            SCHED_WRITE = SchedGen(iterator_support=self.default_iterator_support,
                                   config_width=self.default_config_width)

            self.add_child(f"{self.ctrl_in}_{i}_for_loop",
                           FOR_LOOP_WRITE,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._write[i])

            # Whatever comes through here should hopefully just pipe through seamlessly
            # addressor modules
            self.add_child(f"{self.ctrl_in}_{i}_addr_gen",
                           ADDR_WRITE,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._write[i],
                           mux_sel=FOR_LOOP_WRITE.ports.mux_sel_out,
                           restart=FOR_LOOP_WRITE.ports.restart,
                           addr_out=self._write_addr[i])

            # scheduler modules
            self.add_child(f"{self.ctrl_in}_{i}_sched_gen",
                           SCHED_WRITE,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=FOR_LOOP_WRITE.ports.mux_sel_out,
                           finished=FOR_LOOP_WRITE.ports.restart,
                           valid_output=self._write[i])

        # Set up addr/cycle gens for output side
        for i in range(self.interconnect_output_ports):

            FOR_LOOP_READ = ForLoop(iterator_support=self.default_iterator_support,
                                    config_width=self.default_config_width)

            ADDR_READ = AddrGen(iterator_support=self.default_iterator_support,
                                config_width=self.default_config_width)
            SCHED_READ = SchedGen(iterator_support=self.default_iterator_support,
                                  config_width=self.default_config_width)

            self.add_child(f"{self.ctrl_out}_{i}_for_loop",
                           FOR_LOOP_READ,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._read[i])

            self.add_child(f"{self.ctrl_out}_{i}_addr_gen",
                           ADDR_READ,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._read[i],
                           mux_sel=FOR_LOOP_READ.ports.mux_sel_out,
                           restart=FOR_LOOP_READ.ports.restart,
                           addr_out=self._read_addr[i])

            self.add_child(f"{self.ctrl_out}_{i}_sched_gen",
                           SCHED_READ,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=FOR_LOOP_READ.ports.mux_sel_out,
                           finished=FOR_LOOP_READ.ports.restart,
                           valid_output=self._read[i])
        # -------------------------------- Delineate new group -------------------------------

        # Now deal with dual_port/single_port madness...
        # self._cen_to_sram = self.output("cen_to_strg", 1, packed=True)
        self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
        self._ren_to_sram = self.output("ren_to_strg", 1, packed=True)
        # self.wire(self._cen_to_sram, self._write.r_or() | self._read.r_or())
        self.wire(self._wen_to_sram, self._write.r_or())
        self.wire(self._ren_to_sram, self._read.r_or())

        for i in range(self.interconnect_output_ports):
            self.wire(self._data_out[i], self._data_from_sram)

        if self.rw_same_cycle:
            # If we can read and write the same cycle we
            # can pretty safeuly assume we have separate read/write ports...

            # Handle write side....
            # Send the address/data based on the lowest writer on the port
            self._wr_addr_to_sram = self.output("wr_addr_out", clog2(self.mem_depth), packed=True)
            pri_enc_wr = get_priority_encode(self, self._write)
            self.wire(self._wr_addr_to_sram, self._write_addr[pri_enc_wr][clog2(self.mem_depth) - 1, 0])
            self.wire(self._data_to_sram, self._data_in[pri_enc_wr])

            # Read side...
            pri_enc_rd = get_priority_encode(self, self._read)
            self._rd_addr_to_sram = self.output("rd_addr_out", clog2(self.mem_depth), packed=True)
            self.wire(self._rd_addr_to_sram, self._read_addr[pri_enc_rd][clog2(self.mem_depth) - 1, 0])

            self.base_ports = [[None, None]]
            tmp0_rdaddr = self.output("tmp0_rdaddr", width=self._rd_addr_to_sram.width)
            tmp0_rden = self.output("tmp0_rden", width=self._ren_to_sram.width)
            self.wire(tmp0_rdaddr, kts.const(0, width=self._rd_addr_to_sram.width))
            self.wire(tmp0_rden, kts.const(0, width=self._ren_to_sram.width))
            rw_port = MemoryPort(MemoryPortType.READWRITE)
            rw_port_intf = rw_port.get_port_interface()
            rw_port_intf['data_in'] = self._data_to_sram
            rw_port_intf['data_out'] = None
            rw_port_intf['write_addr'] = self._wr_addr_to_sram
            rw_port_intf['write_enable'] = self._wen_to_sram
            rw_port_intf['read_addr'] = tmp0_rdaddr
            rw_port_intf['read_enable'] = tmp0_rden
            rw_port.annotate_port_signals()
            self.base_ports[0][0] = rw_port
            # Populate second port as just R
            r_port = MemoryPort(MemoryPortType.READ)
            r_port_intf = r_port.get_port_interface()
            r_port_intf['data_out'] = self._data_from_sram
            r_port_intf['read_addr'] = self._rd_addr_to_sram
            r_port_intf['read_enable'] = self._ren_to_sram
            r_port.annotate_port_signals()
            self.base_ports[0][1] = r_port

        else:
            self._addr_to_sram = self.output("addr_out", clog2(self.mem_depth), packed=True)
            pri_enc_wr = get_priority_encode(self, self._write)
            pri_enc_rd = get_priority_encode(self, self._read)
            self.wire(self._data_to_sram, self._data_in[0])
            self.wire(self._addr_to_sram, self._addr)

            @always_comb
            def set_sram_addr():
                if self._write:
                    self._addr = self._write_addr[pri_enc_wr][clog2(self.mem_depth) - 1, 0]
                else:
                    self._addr = self._read_addr[pri_enc_rd][clog2(self.mem_depth) - 1, 0]
            self.add_code(set_sram_addr)

            rw_port = MemoryPort(MemoryPortType.READWRITE)
            rw_port_intf = rw_port.get_port_interface()
            rw_port_intf['data_in'] = self._data_to_sram
            rw_port_intf['data_out'] = self._data_from_sram
            rw_port_intf['write_addr'] = self._addr_to_sram
            rw_port_intf['write_enable'] = self._wen_to_sram
            rw_port_intf['read_addr'] = self._addr_to_sram
            rw_port_intf['read_enable'] = self._ren_to_sram
            rw_port.annotate_port_signals()
            self.base_ports[0][0] = rw_port

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def delay_read(self):
        if ~self._rst_n:
            self._read_d1 = 0
        else:
            self._read_d1 = self._read

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def increment_cycle_count(self):
        if ~self._rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1

    def get_static_bitstream(self, config_path, in_file_name, out_file_name):

        config = []
        in_ctrls = [f"{self.ctrl_in}_{i}" for i in range(self.interconnect_input_ports)]
        out_ctrls = [f"{self.ctrl_out}_{i}" for i in range(self.interconnect_output_ports)]
        controller_objs = {}
        for c in in_ctrls:
            in_path = config_path + '/' + in_file_name + c + '.csv'
            if os.path.isfile(in_path):
                controller_objs[c] = (map_controller(extract_controller(in_path), c), 0)
            else:
                controller_objs[c] = None
                print(f"No {c} file provided. Is this expected?")
        for c in out_ctrls:
            out_path = config_path + '/' + out_file_name + c + '.csv'
            if os.path.isfile(out_path):
                controller_objs[c] = (map_controller(extract_controller(out_path), c), 1)
            else:
                controller_objs[c] = None
                print(f"No {c} file provided. Is this expected?")

        for c_name, c_conf in controller_objs.items():
            print(f"name: {c_name}, controller: {c_conf}")
            config += configure_controller(prefix="strg_ub_",
                                           name=c_name,
                                           controller=c_conf)

        return config

    def get_bitstream(self, config_json, prefix=""):
        # return super().get_bitstream(config_json, prefix=prefix)
        config = []
        in_ctrls = [f"{self.ctrl_in}_{i}" for i in range(self.interconnect_input_ports)]
        out_ctrls = [f"{self.ctrl_out}_{i}" for i in range(self.interconnect_output_ports)]
        for in_ctrl in in_ctrls:
            if in_ctrl in config_json:
                controller_tmp = (map_controller(extract_controller_json(config_json[in_ctrl]), in_ctrl), 0)
                config += configure_controller(prefix="", name=in_ctrl, controller=controller_tmp)
        for out_ctrl in out_ctrls:
            if out_ctrl in config_json:
                controller_tmp = (map_controller(extract_controller_json(config_json[out_ctrl]), out_ctrl), 1)
                config += configure_controller(prefix="", name=out_ctrl, controller=controller_tmp)
        return config

    def get_memory_ports(self):
        return self.base_ports

    def get_inputs(self):
        return super().get_inputs()

    def get_outputs(self):
        return super().get_outputs()

    def __str__(self):
        return self.name

    def get_config_mode_str(self):
        return "UB"


if __name__ == "__main__":
    lake_dut = StrgUBThin()
    verilog(lake_dut, filename="strg_ub_thin.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
