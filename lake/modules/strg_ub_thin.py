from lake.modules.chain_accessor import ChainAccessor
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
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import *
import os
import kratos as kts
from _kratos import create_wrapper_flatten
from lake.utils.util import trim_config_list


class StrgUBThin(MemoryController):
    def __init__(self,
                 config_mode_str="pond",
                 data_width=16,  # CGRA Params
                 mem_width=16,
                 mem_depth=32,
                 input_addr_iterator_support=4,
                 input_sched_iterator_support=4,
                 output_addr_iterator_support=4,
                 output_sched_iterator_support=4,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 config_width=16,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=True,
                 gen_addr=True,
                 comply_with_17=True,
                 area_opt=True,
                 area_opt_share=False,
                 area_opt_dual_config=True,
                 chaining=False,
                 reduced_id_config_width=11,
                 name_suffix="",
                 delay_width=4,
                 iterator_support2=2,  # assumes that this port has smaller iter_support
                 add_flush=False
                 ):

        super().__init__(f"strg_ub_thin{name_suffix}", debug=True, add_flush=add_flush)

        assert mem_width == data_width, f"This module should only be used when the fetch width is 1!"

        self.ctrl_in = "in2regfile"
        self.ctrl_out = "regfile2out"

        self.config_mode_str = config_mode_str
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
        self.comply_with_17 = comply_with_17
        self.area_opt = area_opt
        self.area_opt_share = area_opt_share
        self.area_opt_dual_config = area_opt_dual_config
        self.chaining = chaining
        self.reduced_id_config_width = reduced_id_config_width
        self.delay_width = delay_width
        self.iterator_support2 = iterator_support2
        self.addr_fifo_depth = 2 ** (delay_width - 1)
        self.default_iterator_support = 6
        self.default_config_width = 16
        self.addr_width = clog2(mem_depth)
        # generation parameters
        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self.base_ports = [[None]]

        self.add_bits = 0
        if self.comply_with_17:
            self.add_bits = 1
        self.core_io_width = self.data_width + self.add_bits
        self.bit_range = (self.data_width - 1, 0)

        self._data_in = self.input("data_in", self.core_io_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        # outputs
        self._data_out = self.output("data_out", self.core_io_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)
        self._data_to_sram = self.output("data_to_strg", self.data_width,
                                         size=self.fetch_width,
                                         packed=True)
        self._data_from_sram = self.input("data_from_strg", self.data_width,
                                          size=self.fetch_width,
                                          packed=True)

        # wires used for comply17
        self._data_in_thin = self.var("data_in_thin", self.data_width,
                                      size=self.interconnect_input_ports,
                                      packed=True,
                                      explicit_array=True)

        for idx in range(self.interconnect_input_ports):
            self.wire(self._data_in_thin[idx], self._data_in[idx][self.data_width - 1, 0])

        self._data_out_thin = self.var("data_out_int_thin", self.data_width,
                                       size=self.interconnect_output_ports,
                                       packed=True,
                                       explicit_array=True)

        # Early out in case...
        if self.gen_addr is False:
            # Pass through write enable, addr data and
            # read enable, addr data
            self._read = self.input("ren_in", 1)
            self._write = self.input("wen_in", 1)
            self._write_addr = self.input("write_addr", self.addr_width)
            self._read_addr = self.input("read_addr", self.addr_width)
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
            if self.comply_with_17:
                for i in range(self.interconnect_output_ports):
                    self.wire(self._data_out[i][self.bit_range], self._data_from_sram[0])
                    for i_ in range(self.add_bits):
                        self.wire(self._data_out[self.data_width + i_], kts.const(0, 1))
                self.wire(self._data_in_thin, self._data_to_sram)
            else:
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
        if self.area_opt and self.area_opt_dual_config:
            self._write = self.var("write", 1)
            self._read = self.var("read", 1)
            self._write_mux_sel_msb = self.var("write_mux_sel_msb", 1)
            self._read_mux_sel_msb = self.var("read_mux_sel_msb", 1)
        else:
            self._write = self.var("write", self.interconnect_input_ports)
            self._read = self.var("read", self.interconnect_output_ports)

        # disable accessor signals in Pond
        if not self.area_opt:
            self._accessor_output = self.output("accessor_output", self.interconnect_output_ports)
            self.wire(self._accessor_output, self._read)

        self._valid_out = self.output("valid_out", self.interconnect_output_ports)
        self._valid_out_int = self.var("valid_out_int", self.interconnect_output_ports)
        if self.read_delay == 1:
            if self.area_opt and self.area_opt_dual_config:
                self._read_d1 = self.var("read_d1", 1)
                self.wire(self._valid_out_int[0], self._read_d1 & ~self._read_mux_sel_msb)
                self.wire(self._valid_out_int[1], self._read_d1 & self._read_mux_sel_msb)
            else:
                self._read_d1 = self.var("read_d1", self.interconnect_output_ports)
                self.wire(self._valid_out_int, self._read_d1)
            self.add_code(self.delay_read)
        else:
            if self.area_opt and self.area_opt_dual_config:
                self.wire(self._valid_out_int[0], self._read & ~self._read_mux_sel_msb)
                self.wire(self._valid_out_int[1], self._read & self._read_mux_sel_msb)
            else:
                self.wire(self._valid_out_int, self._read)

        self._data_out_int = self.var("data_out_int", self.data_width,
                                      size=self.interconnect_output_ports,
                                      packed=True,
                                      explicit_array=True)

        if self.chaining:
            # Add chaining in here... since we only use in the UB case...
            self._chain_data_in = self.input("chain_data_in",
                                             self.core_io_width,
                                             size=self.interconnect_output_ports,
                                             packed=True,
                                             explicit_array=True)

            self._chain_data_in_thin = self.var("chain_data_in_thin",
                                                self.data_width,
                                                size=self.interconnect_output_ports,
                                                packed=True,
                                                explicit_array=True)

            for idx in range(self.interconnect_input_ports):
                self.wire(self._chain_data_in_thin[idx], self._chain_data_in[idx][self.data_width - 1, 0])

            chaining = ChainAccessor(data_width=self.data_width,
                                     interconnect_output_ports=self.interconnect_output_ports)

            self.add_child(f"chain", chaining,
                           curr_tile_data_out=self._data_out_int,
                           chain_data_in=self._chain_data_in_thin,
                           accessor_output=self._valid_out_int,
                           data_out_tile=self._data_out_thin)
        else:
            self.wire(self._data_out_thin, self._data_out_int)

        for idx in range(self.interconnect_output_ports):
            self.wire(self._data_out[idx][self.bit_range], self._data_out_thin[idx])
            if self.comply_with_17:
                for i_ in range(self.add_bits):
                    self.wire(self._data_out[idx][self.data_width + i_], kts.const(0, 1))

        self.wire(self._valid_out, self._valid_out_int)

        if self.area_opt and self.area_opt_dual_config:
            self._write_addr = self.var("write_addr", self.addr_width)
            self._read_addr = self.var("read_addr", self.addr_width)
        else:
            self._write_addr = self.var("write_addr", self.addr_width, size=self.interconnect_input_ports, explicit_array=True)
            self._read_addr = self.var("read_addr", self.addr_width, size=self.interconnect_output_ports, explicit_array=True)
        self._addr = self.var("addr", clog2(self.mem_depth))

        # Set up addr/cycle gens for input side
        for i in range(self.interconnect_input_ports):

            if self.area_opt and self.area_opt_dual_config and i == 1:
                break

            if self.area_opt and i == 0:
                FOR_LOOP_WRITE = ForLoop(iterator_support=self.input_sched_iterator_support,
                                         config_width=self.reduced_id_config_width,
                                         dual_config=self.area_opt_dual_config,
                                         iterator_support2=self.iterator_support2)
                ADDR_WRITE = AddrGen(iterator_support=self.input_addr_iterator_support,
                                     config_width=self.addr_width,
                                     dual_config=self.area_opt_dual_config,
                                     iterator_support2=self.iterator_support2)
                SCHED_WRITE = SchedGen(iterator_support=self.input_sched_iterator_support,
                                       config_width=self.default_config_width,
                                       dual_config=self.area_opt_dual_config,
                                       iterator_support2=self.iterator_support2)
            elif self.area_opt and i == 1:
                FOR_LOOP_WRITE = ForLoop(iterator_support=2,
                                         config_width=self.reduced_id_config_width,
                                         dual_config=self.area_opt_dual_config,
                                         iterator_support2=self.iterator_support2)
                ADDR_WRITE = AddrGen(iterator_support=2,
                                     config_width=self.addr_width,
                                     dual_config=self.area_opt_dual_config,
                                     iterator_support2=self.iterator_support2)
                SCHED_WRITE = SchedGen(iterator_support=2,
                                       config_width=self.default_config_width,
                                       dual_config=self.area_opt_dual_config,
                                       iterator_support2=self.iterator_support2)
            else:
                FOR_LOOP_WRITE = ForLoop(iterator_support=self.input_sched_iterator_support,
                                         config_width=self.default_config_width)
                ADDR_WRITE = AddrGen(iterator_support=self.input_addr_iterator_support,
                                     config_width=self.addr_width)
                SCHED_WRITE = SchedGen(iterator_support=self.input_sched_iterator_support,
                                       config_width=self.default_config_width)

            if self.area_opt and self.area_opt_share and i == 1:
                # delay configuration register
                self._delay = self.input(f"delay_{i}", self.delay_width)
                self._delay.add_attribute(ConfigRegAttr("Delay cycles of shared ctrl for update operation"))
                self._delay.add_attribute(FormalAttr(f"{self._delay.name}_{i}", FormalSignalConstraint.SOLVE))

                self._read_shift = self.var(f"read_shift_{i}", 2 ** self.delay_width)
                self._addr_fifo = self.var(f"addr_fifo{i}", self.addr_width,
                                           size=self.addr_fifo_depth,
                                           packed=True,
                                           explicit_array=True)
                self._wr_ptr = self.var(f"wr_ptr_{i}", clog2(self.addr_fifo_depth))
                self._rd_ptr = self.var(f"rd_ptr_{i}", clog2(self.addr_fifo_depth))
                self._ctrl_en = self.var(f"ctrl_en_{i}", 1)
                self._delayed_ctrl_en = self.var(f"delayed_ctrl_en_{i}", 1)
                self._ctrl_addr = self.var(f"ctrl_addr_{i}", self.addr_width)
                self._delayed_ctrl_addr = self.var(f"delayed_ctrl_addr_{i}", self.addr_width)

                @always_ff((posedge, "clk"), (negedge, "rst_n"))
                def update_delayed_ctrl(self):
                    if ~self._rst_n:
                        self._wr_ptr = 0
                        self._rd_ptr = 0
                        self._read_shift = 0
                        self._addr_fifo = 0
                    elif self._delay > 0:
                        # wen shift register
                        self._read_shift = concat(self._read_shift[self._read_shift.width - 2, 1], self._ctrl_en, const(0, 1))

                        # addr fifo
                        if self._ctrl_en:
                            self._addr_fifo[self._wr_ptr] = self._ctrl_addr
                            self._wr_ptr = self._wr_ptr + 1

                        if self._delayed_ctrl_en:
                            self._rd_ptr = self._rd_ptr + 1

                self.add_code(update_delayed_ctrl)
                self.wire(self._delayed_ctrl_en, self._read_shift[self._delay])
                self.wire(self._delayed_ctrl_addr, self._addr_fifo[self._rd_ptr])
                self.wire(self._write[i], self._delayed_ctrl_en)
                self.wire(self._write_addr[i], self._delayed_ctrl_addr)
            else:
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
                               restart=FOR_LOOP_WRITE.ports.restart)

                # scheduler modules
                self.add_child(f"{self.ctrl_in}_{i}_sched_gen",
                               SCHED_WRITE,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               cycle_count=self._cycle_count,
                               mux_sel=FOR_LOOP_WRITE.ports.mux_sel_out,
                               finished=FOR_LOOP_WRITE.ports.restart,
                               valid_output=self._write[i])

                if self.area_opt and self.area_opt_dual_config:
                    self.wire(self._write_addr, ADDR_WRITE.ports.addr_out)
                    self.wire(SCHED_WRITE.ports.mux_sel_msb_init, FOR_LOOP_WRITE.ports.mux_sel_msb_init)
                    self.wire(SCHED_WRITE.ports.mux_sel_msb_init, ADDR_WRITE.ports.mux_sel_msb_init)
                    self.wire(self._write_mux_sel_msb,
                              FOR_LOOP_WRITE.ports.mux_sel_out[FOR_LOOP_WRITE.ports.mux_sel_out.width - 1])
                else:
                    self.wire(self._write_addr[i], ADDR_WRITE.ports.addr_out)

        # Set up addr/cycle gens for output side
        for i in range(self.interconnect_output_ports):

            if self.area_opt and self.area_opt_dual_config and i == 1:
                break

            if self.area_opt and i == 0:
                FOR_LOOP_READ = ForLoop(iterator_support=self.input_sched_iterator_support,
                                        config_width=self.reduced_id_config_width,
                                        dual_config=self.area_opt_dual_config,
                                        iterator_support2=self.iterator_support2)
                ADDR_READ = AddrGen(iterator_support=self.input_addr_iterator_support,
                                    config_width=self.addr_width,
                                    dual_config=self.area_opt_dual_config,
                                    iterator_support2=self.iterator_support2)
                SCHED_READ = SchedGen(iterator_support=self.input_sched_iterator_support,
                                      config_width=self.default_config_width,
                                      dual_config=self.area_opt_dual_config,
                                      iterator_support2=self.iterator_support2)
            elif self.area_opt and i == 1 and (not self.area_opt_dual_config):
                FOR_LOOP_READ = ForLoop(iterator_support=2,
                                        config_width=self.reduced_id_config_width,
                                        dual_config=self.area_opt_dual_config,
                                        iterator_support2=self.iterator_support2)
                ADDR_READ = AddrGen(iterator_support=2,
                                    config_width=self.addr_width,
                                    dual_config=self.area_opt_dual_config,
                                    iterator_support2=self.iterator_support2)
                SCHED_READ = SchedGen(iterator_support=2,
                                      config_width=self.default_config_width,
                                      dual_config=self.area_opt_dual_config,
                                      iterator_support2=self.iterator_support2)
            else:
                FOR_LOOP_READ = ForLoop(iterator_support=self.input_sched_iterator_support,
                                        config_width=self.default_config_width)
                ADDR_READ = AddrGen(iterator_support=self.input_addr_iterator_support,
                                    config_width=self.addr_width)
                SCHED_READ = SchedGen(iterator_support=self.input_sched_iterator_support,
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
                           restart=FOR_LOOP_READ.ports.restart)

            self.add_child(f"{self.ctrl_out}_{i}_sched_gen",
                           SCHED_READ,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=FOR_LOOP_READ.ports.mux_sel_out,
                           finished=FOR_LOOP_READ.ports.restart,
                           valid_output=self._read[i])

            if self.area_opt and self.area_opt_dual_config:
                self.wire(self._read_addr, ADDR_READ.ports.addr_out)
                self.wire(SCHED_READ.ports.mux_sel_msb_init, FOR_LOOP_READ.ports.mux_sel_msb_init)
                self.wire(SCHED_READ.ports.mux_sel_msb_init, ADDR_READ.ports.mux_sel_msb_init)
                self.wire(self._read_mux_sel_msb,
                          FOR_LOOP_READ.ports.mux_sel_out[FOR_LOOP_READ.ports.mux_sel_out.width - 1])
            else:
                self.wire(self._read_addr[i], ADDR_READ.ports.addr_out)

        if self.area_opt and self.area_opt_share and i == 1:
            self.wire(self._ctrl_en, SCHED_READ.ports.valid_output)
            self.wire(self._ctrl_addr, ADDR_READ.ports.addr_out)
        # -------------------------------- Delineate new group -------------------------------

        # Now deal with dual_port/single_port madness...
        # self._cen_to_sram = self.output("cen_to_strg", 1, packed=True)
        self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
        self._ren_to_sram = self.output("ren_to_strg", 1, packed=True)
        # self.wire(self._cen_to_sram, self._write.r_or() | self._read.r_or())
        self.wire(self._wen_to_sram, self._write.r_or())
        self.wire(self._ren_to_sram, self._read.r_or())

        for i in range(self.interconnect_output_ports):
            self.wire(self._data_out_int[i], self._data_from_sram)

        if self.rw_same_cycle:
            # If we can read and write the same cycle we
            # can pretty safeuly assume we have separate read/write ports...

            # Handle write side....
            # Send the address/data based on the lowest writer on the port
            self._wr_addr_to_sram = self.output("wr_addr_out", clog2(self.mem_depth), packed=True)
            if self.area_opt and self.area_opt_dual_config:
                pri_enc_wr = self._write_mux_sel_msb
                self.wire(self._wr_addr_to_sram, self._write_addr[clog2(self.mem_depth) - 1, 0])
                self.wire(self._data_to_sram, self._data_in_thin[pri_enc_wr])
            else:
                pri_enc_wr = get_priority_encode(self, self._write)
                self.wire(self._wr_addr_to_sram, self._write_addr[pri_enc_wr][clog2(self.mem_depth) - 1, 0])
                self.wire(self._data_to_sram, self._data_in_thin[pri_enc_wr])

            # Read side...
            self._rd_addr_to_sram = self.output("rd_addr_out", clog2(self.mem_depth), packed=True)
            if self.area_opt and self.area_opt_dual_config:
                self.wire(self._rd_addr_to_sram, self._read_addr[clog2(self.mem_depth) - 1, 0])
            else:
                pri_enc_rd = get_priority_encode(self, self._read)
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
            self.wire(self._data_to_sram, self._data_in_thin[0])
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

        if self.add_flush:
            self.add_flush_pass()

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

        if self.area_opt:
            for i in range(self.interconnect_input_ports):
                in_ctrl = f"{self.ctrl_in}_{i}"
                if in_ctrl in config_json:
                    controller_tmp = (map_controller(extract_controller_json(config_json[in_ctrl]), in_ctrl), 0)
                    if self.area_opt_dual_config:
                        ctrl_name = f"{self.ctrl_in}_0"
                        ctrl_suffix = ""
                        if i == 1:
                            ctrl_suffix = "2"
                    else:
                        ctrl_name = in_ctrl
                        ctrl_suffix = ""
                    config += configure_controller(prefix="", name=ctrl_name, suffix=ctrl_suffix, controller=controller_tmp)

            for i in range(self.interconnect_output_ports):
                out_ctrl = f"{self.ctrl_out}_{i}"
                if out_ctrl in config_json:
                    controller_tmp = (map_controller(extract_controller_json(config_json[out_ctrl]), out_ctrl), 1)
                    if self.area_opt_dual_config:
                        ctrl_name = f"{self.ctrl_out}_0"
                        ctrl_suffix = ""
                        if i == 1:
                            ctrl_suffix = "2"
                    else:
                        ctrl_name = out_ctrl
                        ctrl_suffix = ""
                    config += configure_controller(prefix="", name=ctrl_name, suffix=ctrl_suffix, controller=controller_tmp)
        else:
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
        print("pond config")
        print(config)
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")
        return trim_config_list(flattened, config)

    def get_memory_ports(self):
        return self.base_ports

    def get_inputs(self):
        return super().get_inputs()

    def get_outputs(self):
        return super().get_outputs()

    def __str__(self):
        return self.name

    def get_config_mode_str(self):
        return self.config_mode_str


if __name__ == "__main__":
    lake_dut = StrgUBThin(read_delay=0, rw_same_cycle=True,
                          area_opt=False,
                          area_opt_dual_config=False,
                          add_flush=True)
    verilog(lake_dut, filename="strg_ub_thin.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
