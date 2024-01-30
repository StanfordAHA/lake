from threading import local
from lake.attributes.formal_attr import *
import json
from kratos import *
from lake.modules.buffet_like import BuffetLike
from lake.modules.crddrop import CrdDrop
# from lake.modules.onyx_pe import OnyxPE
from lake.modules.passthru import *
from lake.modules.strg_ub_vec import StrgUBVec
from lake.modules.strg_ub_thin import StrgUBThin
from lake.modules.strg_fifo import StrgFIFO
from lake.modules.strg_RAM import StrgRAM
from lake.modules.scanner import Scanner
from lake.modules.write_scanner import WriteScanner
from lake.modules.scanner_pipe import ScannerPipe
from lake.modules.intersect import Intersect
from lake.top.extract_tile_info import extract_top_config
from lake.utils.sram_macro import SRAMMacroInfo
import argparse
from lake.top.memtile_builder import MemoryTileBuilder
from lake.top.tech_maps import GF_Tech_Map, SKY_Tech_Map, TSMC_Tech_Map, Intel_Tech_Map
from lake.top.memory_interface import MemoryInterface, MemoryPort, MemoryPortType
from lake.modules.stencil_valid import StencilValid
from _kratos import create_wrapper_flatten
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.fiber_access import FiberAccess
from lake.modules.repeat import Repeat
from lake.modules.repeat_signal_generator import RepeatSignalGenerator
from lake.modules.reg_cr import Reg
from lake.modules.crdhold import CrdHold
import os


class CoreCombiner(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 config_width=16,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sim_sram=True,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 rf=False,
                 config_data_width=32,
                 config_addr_width=8,
                 add_clk_enable=True,
                 add_flush=True,
                 name="CoreCombiner",
                 do_config_lift=True,
                 controllers=None,
                #  tech_map=,
                 tech_map_name='TSMC',
                 io_prefix="",
                 fifo_depth=2,
                 sram_columns=2,
                 ready_valid=True):
        super().__init__(name, debug=True)

        self.ready_valid = ready_valid
        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.config_width = config_width
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.use_sim_sram = use_sim_sram
        assert self.mem_width >= self.data_width, "Data width needs to be smaller than mem"
        self.fw_int = int(self.mem_width / self.data_width)
        self.config_data_width = config_data_width
        self.config_addr_width = config_addr_width
        self.read_delay = read_delay
        self.rw_same_cycle = rw_same_cycle
        self.tech_map_name = tech_map_name
        self.io_prefix = io_prefix
        self.fifo_depth = fifo_depth
        self.rf = rf
        self.sram_columns = sram_columns

        self.data_words_per_set = 2 ** self.config_addr_width
        self.sets = int((self.fw_int * self.mem_depth) / self.data_words_per_set)

        self.sets_per_macro = max(1, int(self.mem_depth / self.data_words_per_set))
        self.total_sets = max(1, self.banks * self.sets_per_macro)

        assert controllers is not None
        # assert len(controllers) > 0
        self.controllers = controllers

        # Create a MemoryTileBuilder
        MTB = MemoryTileBuilder(name, True, io_prefix=self.io_prefix, fifo_depth=self.fifo_depth, ready_valid=self.ready_valid)

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
            tsmc_mem = [MemoryPort(MemoryPortType.READWRITE, delay=self.read_delay, active_read=not rf),
                        MemoryPort(MemoryPortType.READ, delay=self.read_delay, active_read=not rf)]

        # tech_map = self.tech_map(self.mem_depth, self.mem_width)

        if self.tech_map_name == 'TSMC':
            self.tech_map = TSMC_Tech_Map(depth=self.mem_depth, width=int(self.mem_width / self.sram_columns))
        elif self.tech_map_name == 'Intel':
            self.tech_map = Intel_Tech_Map(depth=self.mem_depth, width=int(self.mem_width / self.sram_columns),
                                           async_reset=~MTB.get_async_reset())

        name_prefix = "sram_sp_" if len(tsmc_mem) == 1 else "sram_dp_"

        MTB.set_memory_interface(name_prefix=name_prefix,
                                 mem_params=memory_params,
                                 ports=tsmc_mem,
                                 sim_macro_n=self.use_sim_sram,
                                 tech_map=self.tech_map)

        # Now add the controllers in...
        for ctrl in self.controllers:
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

    def get_port_remap(self):
        return self.dut.get_port_remap()

    def get_modes_supported(self):
        return self.dut.get_modes_supported()

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

    def get_config_mapping(self):
        return self.dut.get_config_mapping()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='LakeTop')
    parser.add_argument("-f",
                        help="optional: will generate verilog, annotation file, and dim to strides/range mapping collateral to solve a formal problem. must provide module to solve for")
    parser.add_argument("--fetch_width", type=int, default=4)
    parser.add_argument("--data_width", type=int, default=16)
    parser.add_argument("--banks", type=int, default=1)
    parser.add_argument("--dual_port", action="store_true")
    parser.add_argument("--fifo_mode", action="store_true")
    parser.add_argument("--stencil_valid", action="store_true")
    parser.add_argument("--rf", action="store_true")
    parser.add_argument("--tech", type=str, default="Intel")

    args = parser.parse_args()

    mem_width = args.fetch_width * 16
    mem_depth = 512
    data_width = args.data_width
    read_delay = 1
    banks = args.banks
    fifo_mode = args.fifo_mode
    stencil_valid = args.stencil_valid
    rf = args.rf
    tech = args.tech

    mem_name = "single"
    rw_same_cycle = False
    if args.dual_port:
        rw_same_cycle = True
        mem_name = "dual"
        if rf:
            read_delay = 0

    fw_int = args.fetch_width

    controllers = []

    pipeline_scanner = False
    fifo_depth = 8

    # if pipeline_scanner:
    #     scan = ScannerPipe(data_width=data_width,
    #                         fifo_depth=fifo_depth,
    #                         add_clk_enable=True,
    #                         defer_fifos=True,
    #                         add_flush=False)
    # else:
    #     scan = Scanner(data_width=data_width,
    #                     fifo_depth=fifo_depth,
    #                     defer_fifos=True,
    #                     add_flush=False)

    # wscan = WriteScanner(data_width=data_width,
    #                         fifo_depth=fifo_depth,
    #                         defer_fifos=True,
    #                         add_flush=False)
    # strg_ub = StrgUBThin(
    #     config_mode_str="pond",
    #     data_width=data_width,  # CGRA Params
    #     mem_width=mem_width,
    #     mem_depth=mem_depth,
    #     input_addr_iterator_support=6,
    #     input_sched_iterator_support=6,
    #     output_addr_iterator_support=6,
    #     output_sched_iterator_support=6,
    #     interconnect_input_ports=1,  # Connection to int
    #     interconnect_output_ports=1,
    #     config_width=16,
    #     read_delay=read_delay,  # Cycle delay in read (SRAM vs Register File)
    #     rw_same_cycle=rw_same_cycle,
    #     gen_addr=True,
    #     comply_with_17=True,
    #     area_opt=False,
    #     area_opt_share=False,
    #     area_opt_dual_config=False,
    #     chaining=True,
    #     reduced_id_config_width=16,
    #     delay_width=4,
    #     iterator_support2=2  # assumes that this port has smaller iter_support
    # )
    strg_ub = StrgUBVec(data_width=16,
                        mem_width=64,
                        mem_depth=512)
    # fiber_access = FiberAccess(data_width=16,
    #                             local_memory=False,
    #                             tech_map=GF_Tech_Map(depth=mem_depth, width=mem_width),
    #                             defer_fifos=True)
    # buffet = BuffetLike(data_width=16, mem_depth=mem_depth, local_memory=False,
    #                     tech_map=GF_Tech_Map(depth=mem_depth, width=mem_width),
    #                     mem_width=mem_width,
    #                     defer_fifos=True,
    #                     optimize_wide=True,
    #                     add_flush=False)
    strg_ram = StrgRAM(data_width=16,
                        banks=1,
                        memory_width=mem_width,
                        memory_depth=mem_depth,
                        rw_same_cycle=False,
                        read_delay=1,
                        addr_width=16,
                        prioritize_write=True,
                        comply_with_17=True)

    stencil_valid = StencilValid()

    # controllers.append(scan)
    # controllers.append(wscan)
    # # controllers.append(buffet)
    controllers.append(strg_ub)
    # # controllers.append(fiber_access)
    # controllers.append(strg_ram)
    # controllers.append(stencil_valid)

    # isect = Intersect(data_width=16,
    #                   use_merger=False,
    #                   fifo_depth=8,
    #                   defer_fifos=True)
    # crd_drop = CrdDrop(data_width=16, fifo_depth=8,
    #                    lift_config=True,
    #                    defer_fifos=True)
    # crd_hold = CrdHold(data_width=16, fifo_depth=8,
    #                            lift_config=True,
    #                            defer_fifos=True,
    #                            add_flush=False)
    # onyxpe = OnyxPE(data_width=16, fifo_depth=8, defer_fifos=True, ext_pe_prefix="pe_prefix")
    # repeat = Repeat(data_width=16,
    #                 fifo_depth=8,
    #                 defer_fifos=True)
    # rsg = RepeatSignalGenerator(data_width=16,
    #                             passthru=False,
    #                             fifo_depth=8,
    #                             defer_fifos=True)
    # regcr = Reg(data_width=16,
    #             fifo_depth=8,
    #             defer_fifos=True)

    # controllers_2 = []

    # controllers_2.append(isect)
    # controllers_2.append(crd_drop)
    # controllers_2.append(crd_hold)
    # controllers_2.append(onyxpe)
    # controllers_2.append(repeat)
    # controllers_2.append(rsg)
    # controllers_2.append(regcr)

    core_comb = CoreCombiner(data_width=16,
                             mem_width=mem_width,
                             mem_depth=mem_depth,
                             banks=1,
                             add_clk_enable=True,
                             add_flush=True,
                             rw_same_cycle=rw_same_cycle,
                             read_delay=read_delay,
                             use_sim_sram=False,
                             controllers=controllers,
                             name=f"CoreCombiner_width_{args.fetch_width}_{mem_name}",
                             do_config_lift=False,
                             io_prefix="MEM_",
                             fifo_depth=16,
                             tech_map_name=tech)

    print(core_comb)
    core_comb_mapping = core_comb.dut.get_port_remap()
    print(core_comb_mapping)
    print(core_comb.get_modes_supported())

    # config = extract_top_config(core_comb.dut, verbose=True)

    # generate verilog
    verilog(core_comb.dut, filename=f"CoreCombiner_width_{args.fetch_width}_{mem_name}.sv",
            optimize_if=False)
