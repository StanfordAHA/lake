from kratos import *
import kratos as kts
from _kratos import create_wrapper_flatten
from math import log

from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.collateral2compiler.memory import mem_inst
from lake.collateral2compiler.helper import *
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.passes.passes import lift_config_reg
from lake.utils.util import safe_wire, trim_config_list
from lake.utils.parse_clkwork_config import * 

# TO DO if write, need to check if that happens for read/write ports
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

        self.stencil_valid = False

        # objects
        self.memories = memories
        self.edges = edges

        self.default_config_width = 16

        # inputs
        self.tile_en = self.input("tile_en", 1)
        self.tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        self.clk_mem = self.clock("clk_mem")
        clk = self.var("clk", 1)
        self.clk = kts.util.clock(clk)
        self.wire(clk, kts.util.clock(self.clk_mem & self.tile_en))
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)

        self.data_in = self.input("data_in",
                                  width=self.word_width,
                                  size=self.input_ports,
                                  explicit_array=True,
                                  packed=True)

        self.data_out = self.output("data_out",
                                    width=self.word_width,
                                    size=self.output_ports,
                                    explicit_array=True,
                                    packed=True)

        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.increment_cycle_count)

        num_mem = len(memories)
        subscript_mems = list(self.memories.keys())

        self.mem_data_outs = [self.var(f"mem_data_out_{i}",
                                       width=self.word_width,
                                       size=self.memories[subscript_mems[i]]["read_port_width" if "read_port_width" in self.memories[subscript_mems[i]] else "read_write_port_width"],
                                       explicit_array=True, packed=True) for i in range(num_mem)]

        self.mem_insts = {}

        i = 0
        for mem in self.memories.keys():
            m = mem_inst(self.memories[mem])
            self.mem_insts[self.memories[mem]["name"]] = m

            self.add_child(self.memories[mem]["name"],
                           m,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           data_out=self.mem_data_outs[i])
            i += 1

        # wire input and output data
        is_input, is_output = [], []
        for mem_name in self.mem_insts.keys():
            make_input, make_output = True, True
            for e in self.edges:
                if mem_name in e["to_signal"]:
                    make_input = False
                elif mem_name in e["from_signal"]:
                    make_output = False
            if make_input:
                is_input.append(mem_name)
            elif make_output:
                is_output.append(mem_name)

        # assume this for now
        assert len(is_input) == self.input_ports
        assert len(is_output) == self.output_ports

        # direct connection to write doesn't work??
        self.high = self.var("high", 1)
        self.wire(self.high, 1)
        self.low = self.var("low", 1)
        self.wire(self.low, 0)

        # wire input data to input memories
        for i in range(len(is_input)):
            in_mem = is_input[i]

            self.valid = self.var(f"input2{in_mem}_accessor_valid", 1)
            self.wire(self.mem_insts[in_mem].ports.write, self.valid)

            safe_wire(self, self.mem_insts[in_mem].ports.data_in[0], self.data_in[i])


            forloop = ForLoop(iterator_support=6,
                              config_width=16)#self.default_config_width)
            loop_itr = forloop.get_iter()
            loop_wth = forloop.get_cfg_width()

            self.add_child(f"input2{in_mem}_forloop",
                           forloop,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid)

            newAG = AddrGen(iterator_support=6,
                            config_width=4)#self.default_config_width)
            self.add_child(f"input2{in_mem}_write_addr_gen",
                           newAG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out,
                           restart=forloop.ports.restart)

            if self.memories[in_mem]["num_read_write_ports"] == 0:
                safe_wire(self, self.mem_insts[in_mem].ports.write_addr[0], newAG.ports.addr_out)
            else:
                safe_wire(self, self.mem_insts[in_mem].ports.read_write_addr[0], newAG.ports.addr_out)

            newSG = SchedGen(iterator_support=6,
                             config_width=self.default_config_width)
            self.add_child(f"input2{in_mem}_write_sched_gen",
                           newSG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           mux_sel=forloop.ports.mux_sel_out,
                           finished=forloop.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self.valid)

        # wire output data from output memories
        for i in range(len(is_output)):
            out_mem = is_output[i]
            self.wire(self.data_out[i], self.mem_data_outs[subscript_mems.index(out_mem)][0])  # , self.mem_insts[out_mem].ports.data_out)

            self.valid = self.var(f"{out_mem}2output_accessor_valid", 1)

            forloop = ForLoop(iterator_support=6,
                              config_width=self.default_config_width)
            loop_itr = forloop.get_iter()
            loop_wth = forloop.get_cfg_width()

            self.add_child(f"{out_mem}2output_forloop",
                           forloop,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid)

            newAG = AddrGen(iterator_support=6,
                            config_width=self.default_config_width)
            self.add_child(f"{out_mem}2output_read_addr_gen",
                           newAG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out,
                           restart=forloop.ports.restart)

            if self.memories[out_mem]["num_read_write_ports"] == 0:
                safe_wire(self, self.mem_insts[out_mem].ports.read_addr[0], newAG.ports.addr_out)
            else:
                safe_wire(self, self.mem_insts[out_mem].ports.read_write_addr[0], newAG.ports.addr_out)

            newSG = SchedGen(iterator_support=6,
                             config_width=self.default_config_width)
            self.add_child(f"{out_mem}2output_read_sched_gen",
                           newSG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           mux_sel=forloop.ports.mux_sel_out,
                           finished=forloop.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self.valid)

        for edge in self.edges:

            # see how many signals need to be selected between for
            # from and to signals for edge
            num_mux_from = len(edge["from_signal"])
            num_mux_to = len(edge["to_signal"])

            # get unique edge_name identifier for hardware modules
            from_sigs, to_sigs = "", ""
            for e in edge["from_signal"]:
                from_sigs += e + "_"
            for e in edge["to_signal"]:
                to_sigs += e + "_"

            edge_name = from_sigs + to_sigs + "edge"

            # create forloop and accessor valid output signal
            self.valid = self.var(edge_name + "_accessor_valid", 1)

            forloop = ForLoop(iterator_support=edge["dim"])
            loop_itr = forloop.get_iter()
            loop_wth = forloop.get_cfg_width()

            self.add_child(edge_name + "_forloop",
                           forloop,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid)

            # create input addressor
            readAG = AddrGen(iterator_support=edge["dim"],
                             config_width=self.default_config_width)
            self.add_child(f"{edge_name}_read_addr_gen",
                           readAG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out,
                           restart=forloop.ports.restart)

            # assign read address to all from memories
            if self.memories[edge["from_signal"][0]]["num_read_write_ports"] == 0:
                # can assign same read addrs to all the memories
                for i in range(len(edge["from_signal"])):
                    # TO DO this should be deleted once mem addressing changes
                    if edge["from_signal"] in ("agg", "agg1"):
                        safe_wire(self, self.mem_insts[edge["from_signal"][i]].ports.read_addr[0], readAG.ports.addr_out*4)
                    else:
                        safe_wire(self, self.mem_insts[edge["from_signal"][i]].ports.read_addr[0], readAG.ports.addr_out)
            else:
                # there needs to be an if valid check here
                for i in range(len(edge["from_signal"])):
                    safe_wire(self, self.mem_insts[edge["from_signal"][i]].ports.read_write_addr[0], readAG.ports.addr_out)

            # if needing to mux, choose which from memory we get data
            # from for to memory data in
            if num_mux_from > 1:
                num_mux_bits = clog2(num_mux_from)
                self.mux_sel = self.var(f"{edge_name}_mux_sel",
                                        width=num_mux_bits)

                safe_wire(self, self.mux_sel,
                          readAG.ports.addr_out[self.mem_insts[edge["from_signal"][0]].ports.read_addr.width + num_mux_from - 1,
                                                self.mem_insts[edge["from_signal"][0]].ports.read_addr.width])

                comb_mux_from = self.combinational()
                # for i in range(num_mux_from):
                if True:
                    if_mux_sel = IfStmt(self.mux_sel == 0)
                    for j in range(len(edge["to_signal"])):
                        # print("TO ", edge["to_signal"][j])
                        # print("FROM ", edge["from_signal"][i])
                        if_mux_sel.then_(self.mem_insts[edge["to_signal"][j]].ports.data_in.assign(self.mem_insts[edge["from_signal"][0]].ports.data_out))
                        # TO DO needed else to get rid of latch, but don't really want it...should really just use a switch statement here
                        if_mux_sel.else_(self.mem_insts[edge["to_signal"][j]].ports.data_in.assign(self.mem_insts[edge["from_signal"][1]].ports.data_out))
                    comb_mux_from.add_stmt(if_mux_sel)

            # no muxing from, data_out from the 1 from memory goes
            # to all to memories (valid determines whether it is
            # actually written)
            else:
                for j in range(len(edge["to_signal"])):
                    # print("TO ", edge["to_signal"][j])
                    # print("FROM ", edge["from_signal"][0])
                    safe_wire(self, self.mem_insts[edge["to_signal"][j]].ports.data_in, self.mem_insts[edge["from_signal"][0]].ports.data_out)

            # create output addressor
            writeAG = AddrGen(iterator_support=edge["dim"],
                              config_width=self.default_config_width)
            self.add_child(f"{edge_name}_write_addr_gen",
                           writeAG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out,
                           restart=forloop.ports.restart)

            # set write addr for to memories
            if self.memories[edge["to_signal"][0]]["num_read_write_ports"] == 0:
                for i in range(len(edge["to_signal"])):
                    # TO DO this should be deleted once mem addressing changes
                    if edge["to_signal"] in ("tb", "tb1"):
                        safe_wire(self, self.mem_insts[edge["to_signal"][i]].ports.write_addr[0], writeAG.ports.addr_out*4)
                    else:
                        safe_wire(self, self.mem_insts[edge["to_signal"][i]].ports.write_addr[0], writeAG.ports.addr_out)
            else:
                for i in range(len(edge["to_signal"])):
                    safe_wire(self, self.mem_insts[edge["to_signal"][i]].ports.read_write_addr[0], writeAG.ports.addr_out)

            # calculate necessary delay between from_signal to to_signal
            # TO DO this may need to be more sophisticated and based on II as well

            if self.memories[edge["from_signal"][0]]["num_read_write_ports"] == 0:
                self.delay = self.memories[edge["from_signal"][0]]["read_info"][0]["latency"]
            else:
                self.delay = self.memories[edge["from_signal"][0]]["read_write_info"][0]["latency"]

            if self.delay > 0:
                self.delayed_writes = self.var(f"{edge_name}_delayed_writes",
                                               width=self.delay)
                self.add_code(self.get_delayed_write)

            # if we have a mux for the two memories, choose who to
            # write to
            if num_mux_to > 1:
                num_mux_bits = clog2(num_mux_to)
                self.mux_sel_to = self.var(f"{edge_name}_mux_sel_to",
                                           width=num_mux_bits)

                safe_wire(self, self.mux_sel_to,
                          writeAG.ports.addr_out[self.mem_insts[edge["to_signal"][0]].ports.write_addr.width + num_mux_to - 1,
                                                 self.mem_insts[edge["to_signal"][0]].ports.write_addr.width])

                comb_mux_to = self.combinational()
                for i in range(num_mux_to):
                    if_mux_sel_to = IfStmt(self.mux_sel_to == i)
                    if self.delay == 0:
                        if_mux_sel_to.then_(self.mem_insts[edge["to_signal"][i]].ports.write.assign(self.valid))
                    else:
                        if_mux_sel_to.then_(self.mem_insts[edge["to_signal"][i]].ports.write.assign(self.delayed_writes[self.delay - 1]))

                    if_mux_sel_to.else_(self.mem_insts[edge["to_signal"][i]].ports.write.assign(self.low))
                    comb_mux_to.add_stmt(if_mux_sel_to)

            # no muxing to, just write to the to memory
            else:
                if self.delay == 0:
                    self.wire(self.mem_insts[edge["to_signal"][0]].ports.write, self.valid)
                else:
                    self.wire(self.mem_insts[edge["to_signal"][0]].ports.write, self.delayed_writes[self.delay - 1])

            # create accessor for edge
            newSG = SchedGen(iterator_support=6,
                             config_width=self.default_config_width)

            self.add_child(edge_name + "_sched_gen",
                           newSG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           mux_sel=forloop.ports.mux_sel_out,
                           finished=forloop.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self.valid)

        lift_config_reg(self.internal_generator)

    # global cycle count for accessor comparison
    @always_ff((posedge, "clk_mem"), (negedge, "rst_n"))
    def increment_cycle_count(self):
        if ~self.rst_n:
            self._cycle_count = 0
        # clking was weird
        elif self.tile_en:
            self._cycle_count = self._cycle_count + 1

    # delay in valid between read from memory and write to next memory
    @always_ff((posedge, "clk_mem"), (negedge, "rst_n"))
    def get_delayed_write(self):
        if ~self.rst_n:
            self.delayed_writes = 0
        else:
            for i in range(self.delay - 1):
                self.delayed_writes[i + 1] = self.delayed_writes[i]
            self.delayed_writes[0] = self.valid

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
            ("agg_agg1_sram_edge_read_addr_gen_starting_addr", agg2sram.out_data_strt),
            ("agg_agg1_sram_edge_write_addr_gen_starting_addr", agg2sram.in_data_strt),
            ("agg_agg1_sram_edge_sched_gen_sched_addr_gen_starting_addr", agg2sram.cyc_strt),
            ("agg_agg1_sram_edge_forloop_dimensionality", agg2sram.dim),

            ("sram_tb_tb1_edge_read_addr_gen_starting_addr", sram2tb.out_data_strt),
            ("sram_tb_tb1_edge_write_addr_gen_starting_addr", sram2tb.in_data_strt),
            ("sram_tb_tb1_edge_sched_gen_sched_addr_gen_starting_addr", sram2tb.cyc_strt),
            ("sram_tb_tb1_edge_forloop_dimensionality", sram2tb.dim),

            ("input2agg_write_addr_gen_starting_addr", in2agg.in_data_strt),
            ("input2agg_write_sched_gen_sched_addr_gen_starting_addr", in2agg.cyc_strt),
            ("input2agg_forloop_dimensionality", in2agg.dim),

            ("tb2output_read_addr_gen_starting_addr", tb2out0.out_data_strt),
            ("tb2output_read_sched_gen_sched_addr_gen_starting_addr", tb2out0.cyc_strt),
            ("tb2output_forloop_dimensionality", tb2out0.dim),

            ("tb12output_read_addr_gen_starting_addr", tb2out1.out_data_strt),
            ("tb12output_read_sched_gen_sched_addr_gen_starting_addr", tb2out1.cyc_strt),
            ("tb12output_forloop_dimensionality", tb2out1.dim),

            # Control Signals...
            ("flush_reg_sel", 0),  # 1
            ("flush_reg_value", 0),  # 1

            # Set the mode and activate the tile...
            ("mode", 0),  # 2
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

        # TODO: Maybe need to check if size 1?
        for i in range(input_ports):
            config.append((f"ren_in_{i}_reg_sel", 1))
            config.append((f"ren_in_{i}_reg_value", 0))

        for i in range(output_ports):
            config.append((f"wen_in_{i}_reg_sel", 1))
            config.append((f"wen_in_{i}_reg_value", 0))

        for i in range(in2agg.dim):
            config.append((f"input2agg_forloop_ranges_{i}", in2agg.extent[i]))
            config.append((f"input2agg_write_addr_gen_strides_{i}", in2agg.in_data_stride[i]))
            config.append((f"input2agg_write_sched_gen_sched_addr_gen_strides_{i}", in2agg.cyc_stride[i]))

        for i in range(agg2sram.dim):
            config.append((f"agg_agg1_sram_edge_read_addr_gen_strides_{i}", agg2sram.out_data_stride[i]))
            config.append((f"agg_agg1_sram_edge_forloop_ranges_{i}", agg2sram.extent[i]))
            config.append((f"agg_agg1_sram_edge_write_addr_gen_strides_{i}", agg2sram.in_data_stride[i]))
            config.append((f"agg_agg1_sram_edge_sched_gen_sched_addr_gen_strides_{i}", agg2sram.cyc_stride[i]))

        tbs = [tb2out0, tb2out1]

        for i in range(sram2tb.dim):
            config.append((f"sram_tb_tb1_edge_forloop_ranges_{i}", sram2tb.extent[i]))
            config.append((f"sram_tb_tb1_edge_read_addr_gen_strides_{i}", sram2tb.out_data_stride[i]))
            config.append((f"sram_tb_tb1_edge_sched_gen_sched_addr_gen_strides_{i}", sram2tb.cyc_stride[i]))
            config.append((f"sram_tb_tb1_edge_write_addr_gen_strides_{i}", sram2tb.in_data_stride[i]))

        tbs = [tb2out0, tb2out1]
        for tb in range(len(tbs)):
            elem = tbs[tb]
            for i in range(elem.dim):
                if tb == 0:
                    config.append((f"tb2output_read_addr_gen_strides_{i}", elem.out_data_stride[i]))
                    config.append((f"tb2output_read_sched_gen_sched_addr_gen_strides_{i}", elem.cyc_stride[i]))
                    config.append((f"tb2output_forloop_ranges_{i}", elem.extent[i]))
                else:
                    config.append((f"tb12output_read_addr_gen_strides_{i}", elem.out_data_stride[i]))
                    config.append((f"tb12output_read_sched_gen_sched_addr_gen_strides_{i}", elem.cyc_stride[i]))
                    config.append((f"tb12output_forloop_ranges_{i}", elem.extent[i]))

        return trim_config_list(flattened, config)
