from lake.attributes.formal_attr import *
import json
from kratos import *
from lake.modules.passthru import *
from lake.modules.strg_ub_vec import StrgUBVec
from lake.modules.strg_ub_thin import StrgUBThin
from lake.modules.strg_fifo import StrgFIFO
from lake.modules.strg_RAM import StrgRAM
from lake.top.extract_tile_info import extract_top_config
from lake.utils.sram_macro import SRAMMacroInfo
import argparse
from lake.top.memtile_builder import MemoryTileBuilder
from lake.top.tech_maps import GF_Tech_Map, SKY_Tech_Map, TSMC_Tech_Map
from lake.top.memory_interface import MemoryInterface, MemoryPort, MemoryPortType
from lake.modules.stencil_valid import StencilValid
from _kratos import create_wrapper_flatten
from lake.attributes.config_reg_attr import ConfigRegAttr
import os


class LakeTop(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 input_iterator_support=6,  # Addr Controllers
                 output_iterator_support=6,
                 config_width=16,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sim_sram=True,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=2,
                 config_data_width=32,
                 config_addr_width=8,
                 num_tiles=1,
                 fifo_mode=True,
                 add_clk_enable=True,
                 add_flush=True,
                 name="LakeTop",
                 gen_addr=True,
                 stencil_valid=True,
                 formal_module=None,
                 do_config_lift=True,
                 tech_map=TSMC_Tech_Map(depth=512, width=32)):
        super().__init__(name, debug=True)

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.input_iterator_support = input_iterator_support
        self.output_iterator_support = output_iterator_support
        self.config_width = config_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.use_sim_sram = use_sim_sram
        self.agg_height = agg_height
        self.input_port_sched_width = clog2(self.interconnect_input_ports)
        assert self.mem_width >= self.data_width, "Data width needs to be smaller than mem"
        self.fw_int = int(self.mem_width / self.data_width)
        self.config_data_width = config_data_width
        self.config_addr_width = config_addr_width
        self.num_tiles = num_tiles
        self.read_delay = read_delay
        self.rw_same_cycle = rw_same_cycle
        self.fifo_mode = fifo_mode
        self.gen_addr = gen_addr
        self.stencil_valid = stencil_valid
        self.formal_module = formal_module
        self.tech_map = tech_map

        self.data_words_per_set = 2 ** self.config_addr_width
        self.sets = int((self.fw_int * self.mem_depth) / self.data_words_per_set)

        self.sets_per_macro = max(1, int(self.mem_depth / self.data_words_per_set))
        self.total_sets = max(1, self.banks * self.sets_per_macro)

        # Create a MemoryTileBuilder
        MTB = MemoryTileBuilder(name, True)

        # For our current implementation, we are just using 1 bank of SRAM
        MTB.set_banks(self.banks)

        # Declare and inject the memory interface for this memory into the MLB
        memory_params = {
            'mem_width': self.mem_width,
            'mem_depth': self.mem_depth
        }

        # Create the memory interface based on different params
        tsmc_mem = [MemoryPort(MemoryPortType.READWRITE, delay=self.read_delay, active_read=True)]

        if self.rw_same_cycle:
            tsmc_mem = [MemoryPort(MemoryPortType.READWRITE, delay=self.read_delay, active_read=True),
                        MemoryPort(MemoryPortType.READ, delay=self.read_delay, active_read=True)]

        # tech_map = self.tech_map(self.mem_depth, self.mem_width)

        name_prefix = "sram_sp_" if len(tsmc_mem) == 1 else "sram_dp_"

        MTB.set_memory_interface(name_prefix=name_prefix,
                                 mem_params=memory_params,
                                 ports=tsmc_mem,
                                 sim_macro_n=self.use_sim_sram,
                                 tech_map=self.tech_map)

        # Now add the controllers in...
        controllers = []

        if self.fw_int > 1:

            controllers.append(StrgUBVec(data_width=self.data_width,
                                         mem_width=self.mem_width,
                                         mem_depth=self.mem_depth,
                                         input_addr_iterator_support=self.input_iterator_support,
                                         input_sched_iterator_support=self.input_iterator_support,
                                         interconnect_input_ports=self.interconnect_input_ports,
                                         interconnect_output_ports=self.interconnect_output_ports,
                                         read_delay=self.read_delay,
                                         rw_same_cycle=self.rw_same_cycle,
                                         agg_height=self.agg_height,
                                         config_width=self.config_width,
                                         agg_data_top=(self.formal_module == "agg")))

        else:

            controllers.append(StrgUBThin(data_width=data_width,
                                          mem_width=self.mem_width,
                                          mem_depth=self.mem_depth,
                                          input_addr_iterator_support=self.input_iterator_support,
                                          input_sched_iterator_support=self.input_iterator_support,
                                          interconnect_input_ports=self.interconnect_input_ports,
                                          interconnect_output_ports=self.interconnect_output_ports,
                                          read_delay=self.read_delay,
                                          rw_same_cycle=self.rw_same_cycle,
                                          config_width=self.config_width))

        if self.fifo_mode and (self.fw_int > 1 or self.banks > 1):
            controllers.append(StrgFIFO(data_width=self.data_width,
                                        banks=self.banks,
                                        memory_width=self.mem_width,
                                        rw_same_cycle=self.rw_same_cycle,
                                        read_delay=self.read_delay,
                                        addr_width=clog2(self.mem_depth)))

        controllers.append(StrgRAM(data_width=self.data_width,
                                   banks=self.banks,
                                   memory_width=self.mem_width,
                                   memory_depth=self.mem_depth,
                                   num_tiles=self.num_tiles,
                                   rw_same_cycle=self.rw_same_cycle,
                                   read_delay=self.read_delay,
                                   addr_width=16,
                                   prioritize_write=True))

        if self.stencil_valid:
            controllers.append(StencilValid())

        for ctrl in controllers:
            MTB.add_memory_controller(ctrl)
        # Finalize number of controllers (so we know how many bits to give the mode register)
        MTB.finalize_controllers()

        # Then add the config hooks...
        MTB.add_config_hooks(config_data_width=self.config_data_width,
                             config_addr_width=self.config_addr_width)

        MTB.realize_hw(clock_gate=add_clk_enable,
                       flush=add_flush,
                       mem_config=True,
                       do_lift_config=do_config_lift)

        self.dut = MTB

    def __str__(self):
        return str(self.dut)

    def form_json(self, config_path):
        config_file = open(config_path, "r")
        loaded_json = json.load(config_file)
        # loaded_json["mode"] = "UB"
        return loaded_json

    def make_wrapper(self, to_wrap, mode="UB", cfg_dict={}, wrapper_name="default_wrapper"):

        replace_ins = {}
        replace_outs = {}

        if mode == "UB" and self.read_delay == 0:
            replace_ins = {
                "input_width_16_num_0": "data_in_pond_0",
                "input_width_16_num_1": "data_in_pond_1",
            }

            replace_outs = {
                "output_width_16_num_0": "data_out_pond_0",
                "output_width_16_num_1": "data_out_pond_1",
                "output_width_1_num_4": "valid_out_pond",
            }
        elif mode == "UB" and self.read_delay >= 1:
            replace_ins = {
                "input_width_16_num_0": "chain_data_in_0",
                "input_width_16_num_1": "chain_data_in_1",
                "input_width_16_num_2": "data_in_0",
                "input_width_16_num_3": "data_in_1",
            }

            replace_outs = {
                "output_width_16_num_0": "data_out_0",
                "output_width_16_num_1": "data_out_1",
                "output_width_1_num_3": "stencil_valid",
            }
        elif mode == "ROM" and self.fw_int > 1:
            replace_ins = {
                "input_width_16_num_0": "chain_data_in_0",
                "input_width_16_num_1": "chain_data_in_1",
                "input_width_16_num_2": "data_in_0",
                "input_width_16_num_3": "data_in_1",
            }

            replace_outs = {
                "output_width_16_num_0": "data_out_0",
                "output_width_16_num_1": "data_out_1",
                "output_width_1_num_3": "stencil_valid",
            }

        else:
            raise NotImplementedError

        # Set the child to external so that it isn't duplicated
        tw_int_gen = to_wrap.internal_generator
        new_gen = Generator(name=wrapper_name)
        new_gen.add_child(f"{tw_int_gen.name}_inst", to_wrap)
        for port_name in tw_int_gen.get_port_names():

            port = tw_int_gen.get_port(port_name)
            # Check if the cfg has a value already assigned
            if port_name in cfg_dict:
                ngv = new_gen.var_from_def(port, port_name)
                if cfg_dict[port_name] < 0:
                    # Make it positive
                    cfg_dict[port_name] = (2 ** ngv.width) + cfg_dict[port_name]
                tmp_val = kts.const(cfg_dict[port_name], ngv.width)
                new_gen.wire(ngv, tmp_val)
                new_gen.wire(ngv, port)
            # Fallback to unassigned config regs to set to 0
            elif len(port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))) == 1:
                ngv = new_gen.var_from_def(port, port_name)
                tmp_val = kts.const(0, ngv.width)
                new_gen.wire(ngv, tmp_val)
                new_gen.wire(ngv, port)
            elif port_name == "clk":
                ngc = new_gen.clock("clk")
                new_gen.wire(ngc, port)
            elif port_name == "clk_en":
                ngc = new_gen.clock_en("clk_en")
                new_gen.wire(ngc, port)
            elif port_name == "rst_n":
                ngc = new_gen.reset("rst_n")
                new_gen.wire(ngc, port)
            elif port_name in replace_ins:
                np = new_gen.port_from_def(port, name=replace_ins[port_name])
                new_gen.wire(np, port)
            elif port_name in replace_outs:
                np = new_gen.port_from_def(port, name=replace_outs[port_name])
                new_gen.wire(np, port)
            else:
                np = new_gen.port_from_def(port, name=port_name)
                new_gen.wire(np, port)
        return new_gen

    def wrapper(self, wrapper_vlog_filename="default_wrapper.sv",
                wrapper_vlog_modulename="LakeTop",
                # vlog_extension="v",
                config_path="/aha/config.json",
                append=False):
        """Create a verilog wrapper for the dut with configurations specified in the json file

        Args:
            vlog_filename (str, optional): Filename for eventual output verilog - default .sv extension. Defaults to "default_wrapper".
            config_path (str, optional): Filepath for configuration json. Defaults to "/aha/config.json".
        """
        # Load the JSON and manipulate before sending under to MTB
        config_json = self.form_json(config_path=config_path)
        mode = config_json['mode']
        configs = self.dut.get_bitstream(config_json=config_json)
        cfg_dict = {}
        for (cfg_reg, val) in configs:
            cfg_dict[cfg_reg] = val

        # get flattened module
        flattened = create_wrapper_flatten(self.dut.internal_generator,
                                           f"{self.dut.name}_flat")
        flattened_gen = Generator(f"{self.dut.name}_flat", internal_generator=flattened)
        # Create another level of wrapping...
        # Set the current dut and flattened dut to external for sharing
        flattened_gen.external = True
        self.dut.external = True

        wrapper = self.make_wrapper(to_wrap=flattened_gen, mode=mode, cfg_dict=cfg_dict,
                                    wrapper_name=wrapper_vlog_modulename)
        # If we want to append, we should generate the the verilog to a temp name and then
        # append it to the original if it exists
        if append:
            if os.path.exists(f"{wrapper_vlog_filename}"):
                new_file_lines = []
                verilog(wrapper, filename=f"tmp_wrapper_vlog")
                with open("tmp_wrapper_vlog", "r") as og_file:
                    new_file_lines = og_file.readlines()
                with open(wrapper_vlog_filename, "a") as cat_file:
                    cat_file.writelines(new_file_lines)
            else:
                verilog(wrapper, filename=f"{wrapper_vlog_filename}")
        else:
            verilog(wrapper, filename=f"{wrapper_vlog_filename}")

        # Restore the external state
        self.dut.external = False

    def get_verilog(self, verilog_name, flattened=False):
        verilog(self.dut, filename=verilog_name, optimize_if=False)
        # Sometimes we need the verilog of the flattened module for a wrapper
        if flattened:
            self.dut.external = True
            flattened = create_wrapper_flatten(self.dut.internal_generator,
                                               f"{self.dut.name}_flat")
            flattened_gen = Generator(f"{self.dut.name}_flat", internal_generator=flattened)
            verilog(flattened_gen, filename=f"{self.dut.name}_flat.v")
            self.dut.external = False


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='LakeTop')
    parser.add_argument("-f",
                        help="optional: will generate verilog, annotation file, and dim to strides/range mapping collateral to solve a formal problem. must provide module to solve for")
    parser.add_argument("--fetch_width", type=int, default=4)
    parser.add_argument("--dual_port", action="store_true")

    args = parser.parse_args()

    mem_width = args.fetch_width * 16

    mem_name = "single"
    rw_same_cycle = False
    if args.dual_port:
        rw_same_cycle = True
        mem_name = "dual"

    lake_top = LakeTop(data_width=16,
                       mem_width=mem_width,
                       mem_depth=512,
                       banks=1,
                       fifo_mode=True,
                       add_clk_enable=True,
                       add_flush=True,
                       rw_same_cycle=False,
                       read_delay=1,
                       use_sim_sram=False,
                       name=f"LakeTop_width_{args.fetch_width}_{mem_name}")

    print(lake_top)

    config = extract_top_config(lake_top.dut, verbose=True)

    # generate verilog
    verilog(lake_top.dut, filename=f"LakeTop_width_{args.fetch_width}_{mem_name}.sv",
            optimize_if=False)
