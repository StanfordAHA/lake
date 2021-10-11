from kratos import *
import kratos as kts
from _kratos import create_wrapper_flatten
from math import log

from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.dsl.helper import *
from lake.dsl.mem_port import MemPort
from lake.dsl.memory import mem_inst
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.passes.passes import lift_config_reg
from lake.utils.util import safe_wire, trim_config_list
from lake.utils.util import extract_formal_annotation_collat, modular_formal_annotation
from lake.utils.parse_clkwork_config import *


class TopLakeHW(Generator):
    def __init__(self,
                 word_width,
                 input_ports,
                 output_ports,
                 memories,
                 edges):

        super().__init__("LakeTop", debug=True)

        # parameters
        self.word_width = word_width
        self.input_ports = input_ports
        self.output_ports = output_ports

        self.default_config_width = 16
        self.cycle_count_width = 16

        self.stencil_valid = False

        # objects
        self.memories = memories
        self.edges = edges

        # tile enable and clock
        self.tile_en = self.input("tile_en", 1)
        self.tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))
        self.tile_en.add_attribute(FormalAttr(self.tile_en.name, FormalSignalConstraint.SET1))

        self.clk_mem = self.clock("clk")
        self.clk_mem.add_attribute(FormalAttr(self.clk_mem.name, FormalSignalConstraint.CLK))

        # chaining
        chain_supported = False
        for mem in self.memories.keys():
            if self.memories[mem]["chaining"]:
                chain_supported = True
                break

        if chain_supported:
            self.chain_en = self.input("chain_en", 1)
            self.chain_en.add_attribute(ConfigRegAttr("Chaining enable"))
            self.chain_en.add_attribute(FormalAttr(self.chain_en.name, FormalSignalConstraint.SET0))
        else:
            self.chain_en = self.var("chain_en", 1)
            self.wire(self.chain_en, 0)

        # gate clock with tile_en
        gclk = self.var("gclk", 1)
        self.gclk = kts.util.clock(gclk)
        self.wire(gclk, self.clk_mem & self.tile_en)

        self.clk_en = self.clock_en("clk_en", 1)

        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)
        self.rst_n.add_attribute(FormalAttr(self.rst_n.name, FormalSignalConstraint.RSTN))

        # data in and out of top level Lake memory object
        self.data_in = self.input("data_in",
                                  width=self.word_width,
                                  size=self.input_ports,
                                  explicit_array=True,
                                  packed=True)
        self.data_in.add_attribute(FormalAttr(self.data_in.name, FormalSignalConstraint.SEQUENCE))

        self.data_out = self.output("data_out",
                                    width=self.word_width,
                                    size=self.output_ports,
                                    explicit_array=True,
                                    packed=True)
        self.data_out.add_attribute(FormalAttr(self.data_out.name, FormalSignalConstraint.SEQUENCE))

        # global cycle count for accessor comparison
        self._cycle_count = self.var("cycle_count", 16)

        @always_ff((posedge, self.gclk), (negedge, "rst_n"))
        def increment_cycle_count(self):
            if ~self.rst_n:
                self._cycle_count = 0
            else:
                self._cycle_count = self._cycle_count + 1

        self.add_always(increment_cycle_count)

        # info about memories
        num_mem = len(memories)
        subscript_mems = list(self.memories.keys())

        # list of the data out from each memory
        self.mem_data_outs = [self.var(f"mem_data_out_{subscript_mems[i]}",
                                       width=self.word_width,
                                       size=self.memories[subscript_mems[i]]
                                       ["read_port_width" if "read_port_width" in self.memories[subscript_mems[i]]
                                        else "read_write_port_width"],
                                       explicit_array=True, packed=True) for i in range(num_mem)]

        # keep track of write, read_addr, and write_addr vars for read/write memories
        # to later check whether there is a write and what to use for the shared port
        self.mem_read_write_addrs = {}

        # create memory instance for each memory
        self.mem_insts = {}
        i = 0
        for mem in self.memories.keys():
            m = mem_inst(self.memories[mem], self.word_width)
            self.mem_insts[mem] = m

            self.add_child(mem,
                           m,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           # put data out in memory data out list
                           data_out=self.mem_data_outs[i],
                           chain_en=self.chain_en)
            i += 1

        # get input and output memories
        is_input, is_output = [], []
        for mem_name in self.memories.keys():
            mem = self.memories[mem_name]
            if mem["is_input"]:
                is_input.append(mem_name)
            if mem["is_output"]:
                is_output.append(mem_name)

        # TODO direct connection to write doesn't work (?), so have to do this...
        self.low = self.var("low", 1)
        self.wire(self.low, 0)

        # TODO adding multiple ports to 1 memory after talking about mux with compiler team

        # set up input memories
        for i in range(len(is_input)):
            in_mem = is_input[i]

            # input addressor / accessor parameters
            input_dim = self.memories[in_mem]["input_edge_params"]["dim"]
            input_range = self.memories[in_mem]["input_edge_params"]["max_range"]
            input_stride = self.memories[in_mem]["input_edge_params"]["max_stride"]
            # input port associated with memory
            input_port_index = self.memories[in_mem]["input_port"]

            self.valid = self.var(
                f"input_port{input_port_index}_2{in_mem}_accessor_valid", 1)
            self.wire(self.mem_insts[in_mem].ports.write, self.valid)

            # hook up data from the specified input port to the memory
            safe_wire(self, self.mem_insts[in_mem].ports.data_in[0],
                      self.data_in[input_port_index])

            if self.memories[in_mem]["num_read_write_ports"] > 0:
                self.mem_read_write_addrs[in_mem] = {"write": self.valid}

            # create IteratorDomain, AddressGenerator, and ScheduleGenerator
            # for writes to this input memory
            forloop = ForLoop(iterator_support=input_dim,
                              config_width=max(1, clog2(input_range)))  # self.default_config_width)
            loop_itr = forloop.get_iter()
            loop_wth = forloop.get_cfg_width()

            self.add_child(f"input_port{input_port_index}_2{in_mem}_forloop",
                           forloop,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           step=self.valid)

            newAG = AddrGen(iterator_support=input_dim,
                            config_width=max(1, clog2(input_stride)))  # self.default_config_width)
            self.add_child(f"input_port{input_port_index}_2{in_mem}_write_addr_gen",
                           newAG,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out,
                           restart=forloop.ports.restart)

            if self.memories[in_mem]["num_read_write_ports"] == 0:
                safe_wire(self, self.mem_insts[in_mem].ports.write_addr[0], newAG.ports.addr_out)
            else:
                self.mem_read_write_addrs[in_mem]["write_addr"] = newAG.ports.addr_out

            newSG = SchedGen(iterator_support=input_dim,
                             config_width=self.cycle_count_width)
            self.add_child(f"input_port{input_port_index}_2{in_mem}_write_sched_gen",
                           newSG,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           mux_sel=forloop.ports.mux_sel_out,
                           finished=forloop.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self.valid)

        # set up output memories
        for i in range(len(is_output)):
            out_mem = is_output[i]

            # output addressor / accessor parameters
            output_dim = self.memories[out_mem]["output_edge_params"]["dim"]
            output_range = self.memories[out_mem]["output_edge_params"]["max_range"]
            output_stride = self.memories[out_mem]["output_edge_params"]["max_stride"]
            # output port associated with memory
            output_port_index = self.memories[out_mem]["output_port"]

            # hook up data from the memory to the specified output port
            self.wire(self.data_out[output_port_index],
                      self.mem_insts[out_mem].ports.data_out[0][0])
            # self.mem_data_outs[subscript_mems.index(out_mem)][0])

            self.valid = self.var(f"{out_mem}2output_port{output_port_index}_accessor_valid", 1)
            if self.memories[out_mem]["rw_same_cycle"]:
                self.wire(self.mem_insts[out_mem].ports.read, self.valid)

            # create IteratorDomain, AddressGenerator, and ScheduleGenerator
            # for reads from this output memory
            forloop = ForLoop(iterator_support=output_dim,
                              config_width=max(1, clog2(output_range)))  # self.default_config_width)
            loop_itr = forloop.get_iter()
            loop_wth = forloop.get_cfg_width()

            self.add_child(f"{out_mem}2output_port{output_port_index}_forloop",
                           forloop,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           step=self.valid)

            newAG = AddrGen(iterator_support=output_dim,
                            config_width=max(1, clog2(output_stride)))  # self.default_config_width)
            self.add_child(f"{out_mem}2output_port{output_port_index}_read_addr_gen",
                           newAG,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out,
                           restart=forloop.ports.restart)

            if self.memories[out_mem]["num_read_write_ports"] == 0:
                safe_wire(self, self.mem_insts[out_mem].ports.read_addr[0], newAG.ports.addr_out)
            else:
                self.mem_read_write_addrs[in_mem]["read_addr"] = newAG.ports.addr_out

            newSG = SchedGen(iterator_support=output_dim,
                             config_width=self.cycle_count_width)  # self.default_config_width)
            self.add_child(f"{out_mem}2output_port{output_port_index}_read_sched_gen",
                           newSG,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           mux_sel=forloop.ports.mux_sel_out,
                           finished=forloop.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self.valid)

        # create shared IteratorDomains and accessors as well as
        # read/write addressors for memories connected by each edge
        for edge in self.edges:

            # see how many signals need to be selected between for
            # from and to signals for edge
            num_mux_from = len(edge["from_signal"])
            num_mux_to = len(edge["to_signal"])

            # get unique edge_name identifier for hardware modules
            edge_name = get_edge_name(edge)

            # create forloop and accessor valid output signal
            self.valid = self.var(edge_name + "_accessor_valid", 1)

            forloop = ForLoop(iterator_support=edge["dim"])
            self.forloop = forloop
            loop_itr = forloop.get_iter()
            loop_wth = forloop.get_cfg_width()

            self.add_child(edge_name + "_forloop",
                           forloop,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           step=self.valid)

            # create input addressor
            readAG = AddrGen(iterator_support=edge["dim"],
                             config_width=self.default_config_width)
            self.add_child(f"{edge_name}_read_addr_gen",
                           readAG,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out,
                           restart=forloop.ports.restart)

            # assign read address to all from memories
            if self.memories[edge["from_signal"][0]]["num_read_write_ports"] == 0:
                # can assign same read addrs to all the memories
                for i in range(len(edge["from_signal"])):
                    safe_wire(self, self.mem_insts[edge["from_signal"][i]].ports.read_addr[0], readAG.ports.addr_out)
            else:
                for i in range(len(edge["from_signal"])):
                    self.mem_read_write_addrs[edge["from_signal"][i]]["read_addr"] = readAG.ports.addr_out

            # if needing to mux, choose which from memory we get data
            # from for to memory data in
            if num_mux_from > 1:
                num_mux_bits = clog2(num_mux_from)
                self.mux_sel = self.var(f"{edge_name}_mux_sel",
                                        width=num_mux_bits)

                read_addr_width = max(1, clog2(self.memories[edge["from_signal"][0]]["capacity"]))
                # decide which memory to get data from for to memory's data in
                safe_wire(self, self.mux_sel,
                          readAG.ports.addr_out[read_addr_width + num_mux_from - 1, read_addr_width])

                comb_mux_from = self.combinational()
                # for i in range(num_mux_from):
                # TODO want to use a switch statement here, but get add_fn_ln issue
                if_mux_sel = IfStmt(self.mux_sel == 0)
                for j in range(len(edge["to_signal"])):
                    # print("TO ", edge["to_signal"][j])
                    # print("FROM ", edge["from_signal"][i])
                    if_mux_sel.then_(self.mem_insts[edge["to_signal"][j]].ports.data_in.assign(self.mem_insts[edge["from_signal"][0]].ports.data_out))
                    if_mux_sel.else_(self.mem_insts[edge["to_signal"][j]].ports.data_in.assign(self.mem_insts[edge["from_signal"][1]].ports.data_out))
                comb_mux_from.add_stmt(if_mux_sel)

            # no muxing from, data_out from the one and only memory
            # goes to all to memories (valid determines whether it is
            # actually written)
            else:
                for j in range(len(edge["to_signal"])):
                    # print("TO ", edge["to_signal"][j])
                    # print("FROM ", edge["from_signal"][0])
                    safe_wire(self,
                              self.mem_insts[edge["to_signal"][j]].ports.data_in,
                              # only one memory to read from
                              self.mem_insts[edge["from_signal"][0]].ports.data_out)

            # create output addressor
            writeAG = AddrGen(iterator_support=edge["dim"],
                              config_width=self.default_config_width)
            # step, mux_sel, restart may need delayed signals (assigned later)
            self.add_child(f"{edge_name}_write_addr_gen",
                           writeAG,
                           clk=self.gclk,
                           rst_n=self.rst_n)

            # set write addr for to memories
            if self.memories[edge["to_signal"][0]]["num_read_write_ports"] == 0:
                for i in range(len(edge["to_signal"])):
                    safe_wire(self, self.mem_insts[edge["to_signal"][i]].ports.write_addr[0], writeAG.ports.addr_out)
            else:
                for i in range(len(edge["to_signal"])):
                    self.mem_read_write_addrs[edge["to_signal"][i]] = {"write": self.valid, "write_addr": writeAG.ports.addr_out}

            # calculate necessary delay between from_signal to to_signal
            # TODO this may need to be more sophisticated and based on II as well
            # TODO just need to add for loops for all the ports
            if self.memories[edge["from_signal"][0]]["num_read_write_ports"] == 0:
                self.delay = self.memories[edge["from_signal"][0]]["read_info"][0]["latency"]
            else:
                self.delay = self.memories[edge["from_signal"][0]]["read_write_info"][0]["latency"]

            if self.delay > 0:
                # signals that need to be delayed due to edge latency
                self.delayed_writes = self.var(f"{edge_name}_delayed_writes",
                                               width=self.delay)
                self.delayed_mux_sels = self.var(f"{edge_name}_delayed_mux_sels",
                                                 width=self.forloop.ports.mux_sel_out.width,
                                                 size=self.delay,
                                                 explicit_array=True,
                                                 packed=True)
                self.delayed_restarts = self.var(f"{edge_name}_delayed_restarts",
                                                 width=self.delay)

                # delay in valid between read from memory and write to next memory
                @always_ff((posedge, self.gclk), (negedge, "rst_n"))
                def get_delayed_write(self):
                    if ~self.rst_n:
                        self.delayed_writes = 0
                        self.delayed_mux_sels = 0
                        self.delayed_restarts = 0
                    else:
                        for i in range(self.delay - 1):
                            self.delayed_writes[i + 1] = self.delayed_writes[i]
                            self.delayed_mux_sels[i + 1] = self.delayed_mux_sels[i]
                            self.delayed_restarts[i + 1] = self.delayed_restarts[i]
                        self.delayed_writes[0] = self.valid
                        self.delayed_mux_sels[0] = self.forloop.ports.mux_sel_out
                        self.delayed_restarts[0] = self.forloop.ports.restart

                self.add_always(get_delayed_write)

            # if we have a mux for the destination memories,
            # choose which mux to write to
            if num_mux_to > 1:
                num_mux_bits = clog2(num_mux_to)
                self.mux_sel_to = self.var(f"{edge_name}_mux_sel_to",
                                           width=num_mux_bits)

                write_addr_width = max(1, clog2(self.memories[edge["to_signal"][0]]["capacity"]))
                # decide which destination memory gets written to
                safe_wire(self, self.mux_sel_to,
                          writeAG.ports.addr_out[write_addr_width + num_mux_to - 1, write_addr_width])

                # wire the write (or if needed, delayed write) signal to the selected destination memory
                # and set write enable low for all other destination memories
                comb_mux_to = self.combinational()
                for i in range(num_mux_to):
                    if_mux_sel_to = IfStmt(self.mux_sel_to == i)
                    if self.delay == 0:
                        if_mux_sel_to.then_(self.mem_insts[edge["to_signal"][i]].ports.write.assign(self.valid))
                    else:
                        if_mux_sel_to.then_(self.mem_insts[edge["to_signal"][i]].ports.write.assign(self.delayed_writes[self.delay - 1]))

                    if_mux_sel_to.else_(self.mem_insts[edge["to_signal"][i]].ports.write.assign(self.low))
                    comb_mux_to.add_stmt(if_mux_sel_to)

            # no muxing to, just write to the one destination memory
            else:
                if self.delay == 0:
                    self.wire(self.mem_insts[edge["to_signal"][0]].ports.write, self.valid)
                else:
                    self.wire(self.mem_insts[edge["to_signal"][0]].ports.write, self.delayed_writes[self.delay - 1])

            # assign delayed signals for write addressor if needed
            if self.delay == 0:
                self.wire(writeAG.ports.step, self.valid)
                self.wire(writeAG.ports.mux_sel, self.forloop.ports.mux_sel_out)
                self.wire(writeAG.ports.restart, self.forloop.ports.restart)
            else:
                self.wire(writeAG.ports.step, self.delayed_writes[self.delay - 1])
                self.wire(writeAG.ports.mux_sel, self.delayed_mux_sels[self.delay - 1])
                self.wire(writeAG.ports.restart, self.delayed_restarts[self.delay - 1])

            # create accessor for edge
            newSG = SchedGen(iterator_support=edge["dim"],
                             config_width=self.cycle_count_width)  # self.default_config_width)

            self.add_child(edge_name + "_sched_gen",
                           newSG,
                           clk=self.gclk,
                           rst_n=self.rst_n,
                           mux_sel=forloop.ports.mux_sel_out,
                           finished=forloop.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self.valid)

        # for read write memories, choose either read or write address based on whether
        # we are writing to the memory (whether write enable is high)
        read_write_addr_comb = self.combinational()
        for mem_name in self.memories:
            if mem_name in self.mem_read_write_addrs:
                mem_info = self.mem_read_write_addrs[mem_name]
                if_write = IfStmt(mem_info["write"] == 1)
                addr_width = self.mem_insts[mem_name].ports.read_write_addr[0].width
                if_write.then_(self.mem_insts[mem_name].ports.read_write_addr[0].assign(mem_info["write_addr"][addr_width - 1, 0]))
                if_write.else_(self.mem_insts[mem_name].ports.read_write_addr[0].assign(mem_info["read_addr"][addr_width - 1, 0]))
                read_write_addr_comb.add_stmt(if_write)

        # clock enable and flush passes
        kts.passes.auto_insert_clock_enable(self.internal_generator)
        clk_en_port = self.internal_generator.get_port("clk_en")
        clk_en_port.add_attribute(FormalAttr(clk_en_port.name, FormalSignalConstraint.SET1))

        self.add_attribute("sync-reset=flush")
        kts.passes.auto_insert_sync_reset(self.internal_generator)
        flush_port = self.internal_generator.get_port("flush")

        # bring config registers up to top level
        lift_config_reg(self.internal_generator)

        # formal subproblem annotations - uncomment to generate relevant files
        # extract_formal_annotation_collat(self, "dsl_annotation.txt", subscript_mems, edges)
        # modular_formal_annotation(self, subscript_mems)

    def get_static_bitstream(self,
                             config_path,
                             in_file_name,
                             out_file_name):

        input_ports = 1
        output_ports = 1

        in2agg = map_controller(extract_controller(config_path + '/' + in_file_name + '_in2agg_0.csv'), "in2agg")
        agg2sram = map_controller(extract_controller(config_path + '/' + in_file_name + '_agg2sram.csv'), "agg2sram")
        sram2tb = map_controller(extract_controller(config_path + '/' + out_file_name + '_2_sram2tb.csv'), "sram2tb")
        tb2out0 = map_controller(extract_controller(config_path + '/' + out_file_name + '_2_tb2out_0.csv'), "tb2out0")
        tb2out1 = map_controller(extract_controller(config_path + '/' + out_file_name + '_2_tb2out_1.csv'), "tb2out1")

        # Getting bitstreams is a little unweildy due to fault (or its underlying implementation) not
        # handling arrays in the interface.
        # To alleviate this, we create the flattened wrapper so we can query widths of config
        # registers and trim values to their bitwidths...
        print(f"Current_name: {self.name}")
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Set configuration...
        config = [
            ("agg0_agg1_sram_edge_read_addr_gen_starting_addr", agg2sram.out_data_strt),
            ("agg0_agg1_sram_edge_write_addr_gen_starting_addr", agg2sram.in_data_strt),
            ("agg0_agg1_sram_edge_sched_gen_sched_addr_gen_starting_addr", agg2sram.cyc_strt),
            ("agg0_agg1_sram_edge_sched_gen_enable", 1),
            ("agg0_agg1_sram_edge_forloop_dimensionality", agg2sram.dim),

            ("sram_tb0_tb1_edge_read_addr_gen_starting_addr", sram2tb.out_data_strt),
            ("sram_tb0_tb1_edge_write_addr_gen_starting_addr", sram2tb.in_data_strt),
            ("sram_tb0_tb1_edge_sched_gen_sched_addr_gen_starting_addr", sram2tb.cyc_strt),
            ("sram_tb0_tb1_edge_sched_gen_enable", 1),
            ("sram_tb0_tb1_edge_forloop_dimensionality", sram2tb.dim),

            ("input_port0_2agg0_write_addr_gen_starting_addr", in2agg.in_data_strt),
            ("input_port0_2agg0_write_sched_gen_sched_addr_gen_starting_addr", in2agg.cyc_strt),
            ("input_port0_2agg0_write_sched_gen_enable", 1),
            ("input_port0_2agg0_forloop_dimensionality", in2agg.dim),

            ("tb02output_port0_read_addr_gen_starting_addr", tb2out0.out_data_strt),
            ("tb02output_port0_read_sched_gen_sched_addr_gen_starting_addr", tb2out0.cyc_strt),
            ("tb02output_port0_read_sched_gen_enable", 1),
            ("tb02output_port0_forloop_dimensionality", tb2out0.dim),

            ("tb12output_port1_read_addr_gen_starting_addr", tb2out1.out_data_strt),
            ("tb12output_port1_read_sched_gen_sched_addr_gen_starting_addr", tb2out1.cyc_strt),
            ("tb12output_port1_read_sched_gen_enable", 1),
            ("tb12output_port1_forloop_dimensionality", tb2out1.dim),

            ("tile_en", 1),  # 1
        ]

        # Check the hardware if it supports stencil valid
        if self.stencil_valid:
            cfg_path = config_path + '/' + 'stencil_valid.csv'
            # Check if the stencil valid file exists...if it doesn't we just won't program it
            if os.path.exists(cfg_path):
                stcl_valid = map_controller(extract_controller(cfg_path), "stencil_valid")
                config.append((f"loops_stencil_valid_dimensionality", stcl_valid.dim))
                config.append((f"stencil_valid_sched_gen_sched_addr_gen_starting_addr", stcl_valid.cyc_strt))
                for i in range(stcl_valid.dim):
                    config.append((f"loops_stencil_valid_ranges_{i}", stcl_valid.extent[i]))
                    config.append((f"stencil_valid_sched_gen_sched_addr_gen_strides_{i}", stcl_valid.cyc_stride[i]))
            else:
                print("No configuration file provided for stencil valid...are you expecting one to exist?")
                print(f"Bogus stencil valid path: {cfg_path}")

        for i in range(in2agg.dim):
            config.append((f"input_port0_2agg0_forloop_ranges_{i}", in2agg.extent[i]))
            config.append((f"input_port0_2agg0_write_addr_gen_strides_{i}", in2agg.in_data_stride[i]))
            config.append((f"input_port0_2agg0_write_sched_gen_sched_addr_gen_strides_{i}", in2agg.cyc_stride[i]))

        for i in range(agg2sram.dim):
            config.append((f"agg0_agg1_sram_edge_read_addr_gen_strides_{i}", agg2sram.out_data_stride[i]))
            config.append((f"agg0_agg1_sram_edge_forloop_ranges_{i}", agg2sram.extent[i]))
            config.append((f"agg0_agg1_sram_edge_write_addr_gen_strides_{i}", agg2sram.in_data_stride[i]))
            config.append((f"agg0_agg1_sram_edge_sched_gen_sched_addr_gen_strides_{i}", agg2sram.cyc_stride[i]))

        tbs = [tb2out0, tb2out1]

        for i in range(sram2tb.dim):
            config.append((f"sram_tb0_tb1_edge_forloop_ranges_{i}", sram2tb.extent[i]))
            config.append((f"sram_tb0_tb1_edge_read_addr_gen_strides_{i}", sram2tb.out_data_stride[i]))
            config.append((f"sram_tb0_tb1_edge_sched_gen_sched_addr_gen_strides_{i}", sram2tb.cyc_stride[i]))
            config.append((f"sram_tb0_tb1_edge_write_addr_gen_strides_{i}", sram2tb.in_data_stride[i]))

        tbs = [tb2out0, tb2out1]
        for tb in range(len(tbs)):
            elem = tbs[tb]
            for i in range(elem.dim):
                if tb == 0:
                    config.append((f"tb02output_port0_read_addr_gen_strides_{i}", elem.out_data_stride[i]))
                    config.append((f"tb02output_port0_read_sched_gen_sched_addr_gen_strides_{i}", elem.cyc_stride[i]))
                    config.append((f"tb02output_port0_forloop_ranges_{i}", elem.extent[i]))
                else:
                    config.append((f"tb12output_port1_read_addr_gen_strides_{i}", elem.out_data_stride[i]))
                    config.append((f"tb12output_port1_read_sched_gen_sched_addr_gen_strides_{i}", elem.cyc_stride[i]))
                    config.append((f"tb12output_port1_forloop_ranges_{i}", elem.extent[i]))

        return trim_config_list(flattened, config)

    # Function for generating Pond API
    def generate_pond_api(self, ctrl_rd, ctrl_wr):

        (tform_ranges_rd, tform_strides_rd) = transform_strides_and_ranges(ctrl_rd[0], ctrl_rd[1], ctrl_rd[2])
        (tform_ranges_wr, tform_strides_wr) = transform_strides_and_ranges(ctrl_wr[0], ctrl_wr[1], ctrl_wr[2])

        (tform_ranges_rd_sched, tform_strides_rd_sched) = transform_strides_and_ranges(ctrl_rd[0], ctrl_rd[5], ctrl_rd[2])
        (tform_ranges_wr_sched, tform_strides_wr_sched) = transform_strides_and_ranges(ctrl_wr[0], ctrl_wr[5], ctrl_wr[2])

        dim_rd = ctrl_rd[2]
        dim_wr = ctrl_wr[2]

        new_config = {}

        new_config["input_port0_2pond_forloop_dimensionality"] = ctrl_wr[2]
        for i in range(len(tform_ranges_wr)):
            new_config[f"input_port0_2pond_forloop_ranges_{i}"] = tform_ranges_wr[i]
        new_config["input_port0_2pond_write_addr_gen_starting_addr"] = ctrl_wr[3]
        for i in range(len(tform_strides_wr)):
            new_config[f"input_port0_2pond_write_addr_gen_strides_{i}"] = tform_strides_wr[i]
        new_config["input_port0_2pond_write_sched_gen_enable"] = 1
        new_config["input_port0_2pond_write_sched_gen_sched_addr_gen_starting_addr"] = ctrl_wr[4]
        for i in range(len(tform_strides_wr_sched)):
            new_config[f"input_port0_2pond_write_sched_gen_sched_addr_gen_strides_{i}"] = tform_strides_wr_sched[i]
        new_config["pond2output_port0_forloop_dimensionality"] = ctrl_rd[2]
        for i in range(len(tform_ranges_rd)):
            new_config[f"pond2output_port0_forloop_ranges_{i}"] = tform_ranges_rd[i]
        new_config["pond2output_port0_read_addr_gen_starting_addr"] = ctrl_rd[3]
        for i in range(len(tform_strides_rd)):
            new_config[f"pond2output_port0_read_addr_gen_strides_{i}"] = tform_strides_rd[i]
        new_config["pond2output_port0_read_sched_gen_enable"] = 1
        new_config["pond2output_port0_read_sched_gen_sched_addr_gen_starting_addr"] = ctrl_rd[4]
        for i in range(len(tform_strides_rd_sched)):
            new_config[f"pond2output_port0_read_sched_gen_sched_addr_gen_strides_{i}"] = tform_strides_rd_sched[i]

        # general configs
        new_config["tile_en"] = 1
        new_config["clk_en"] = 1
        return new_config


if __name__ == "__main__":
    # this is auto generated after top_lake pre-processing for inputs
    # for hardware generation
    memories = {
        'agg': {
            'name': 'agg',
            'capacity': 4,
            'use_macro': False,
            'word_width': 16,
            'is_input': True,
            'input_edge_params': {"dim": 6, "max_range": 2**16, "max_stride": 2**16},
            'input_port': 0,
            'is_output': False,
            'read_port_width': 4,
            'write_port_width': 1,
            'num_write_ports': 1,
            'num_read_ports': 1,
            'num_read_write_ports': 0,
            'read_ports': [
                MemPort(
                    0, 0)],
            'write_ports': [
                MemPort(
                    1, 0)],
            'read_write_ports': []},
        'agg1': {
            'name': 'agg1',
            'capacity': 4,
            'use_macro': False,
            'word_width': 16,
            'is_input': True,
            'input_edge_params': {"dim": 6, "max_range": 2**16, "max_stride": 2**16},
            'input_port': 1,
            'is_output': False,
            'read_port_width': 4,
            'write_port_width': 1,
            'num_write_ports': 1,
            'num_read_ports': 1,
            'num_read_write_ports': 0,
            'read_ports': [
                MemPort(
                    0, 0)],
            'write_ports': [
                MemPort(
                    1, 0)],
            'read_write_ports': []},
        'sram': {
            'name': 'sram',
            'capacity': 512,
            'use_macro': True,
            'word_width': 16,
            'is_input': False,
            'is_output': False,
            'read_write_port_width': 4,
            'num_write_ports': 0,
            'num_read_ports': 0,
            'num_read_write_ports': 1,
            'read_ports': [],
            'write_ports': [],
            'read_write_ports': [
                MemPort(
                    1, 0)]},
        'tb': {
            'name': 'tb',
            'capacity': 8,
            'use_macro': False,
            'word_width': 16,
            'is_input': False,
            'is_output': True,
            'output_edge_params': {"dim": 6, "max_range": 2**16, "max_stride": 2**16},
            'output_port': 0,
            'read_port_width': 1,
            'write_port_width': 4,
            'num_write_ports': 1,
            'num_read_ports': 1,
            'num_read_write_ports': 0,
            'read_ports': [
                MemPort(
                    0, 0)],
            'write_ports': [
                MemPort(
                    1, 0)],
            'read_write_ports': []},
        'tb1': {
            'name': 'tb1',
            'capacity': 8,
            'use_macro': False,
            'word_width': 16,
            'is_input': False,
            'is_output': True,
            'output_edge_params': {"dim": 6, "max_range": 2**16, "max_stride": 2**16},
            'output_port': 1,
            'read_port_width': 1,
            'write_port_width': 4,
            'num_write_ports': 1,
            'num_read_ports': 1,
            'num_read_write_ports': 0,
            'read_ports': [
                MemPort(
                    0, 0)],
            'write_ports': [
                MemPort(
                    1, 0)],
            'read_write_ports': []}}

    edges = [
        {'from_signal': ['agg', 'agg1'],
            'to_signal': ['sram'],
            'dim': 6,
            'max_range': 65535,
            'max_stride': 65535},
        {'from_signal': ['sram'],
            'to_signal': ['tb', 'tb1'],
            'dim': 6,
            'max_range': 65535,
            'max_stride': 65535}]

    tile = TopLakeHW(word_width=16,
                     input_ports=2,
                     output_ports=2,
                     memories=memories,
                     edges=edges)

    verilog(tile, filename="Lake_hw.sv",
            check_multiple_driver=False,
            optimize_if=False,
            check_flip_flop_always_ff=False)

    Generator.clear_context_hash()

    magma_dut = kts.util.to_magma(tile,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)
