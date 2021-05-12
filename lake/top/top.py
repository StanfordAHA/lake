from lake.modules.stencil_valid import StencilValid
from lake.top.tech_maps import TSMC_Tech_Map
from lake.top.memory_interface import MemoryInterface, MemoryPort, MemoryPortType
from lake.attributes.formal_attr import *
import os
from kratos import *
from lake.modules.passthru import *
from lake.modules.strg_ub_vec import StrgUBVec
from lake.modules.strg_ub_thin import StrgUBThin
from lake.modules.strg_RAM import StrgRAM
from lake.utils.sram_macro import SRAMMacroInfo
from _kratos import create_wrapper_flatten
import argparse
from lake.top.memtile_builder import MemoryTileBuilder


class Top():
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 input_iterator_support=6,  # Addr Controllers
                 output_iterator_support=6,
                 input_config_width=16,
                 output_config_width=16,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sram_stub=True,
                 sram_macro_info=SRAMMacroInfo("tsmc_name"),
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4,
                 config_data_width=32,
                 config_addr_width=8,
                 num_tiles=1,
                 fifo_mode=False,
                 add_clk_enable=True,
                 add_flush=True,
                 name="mek",
                 gen_addr=True,
                 stencil_valid=True,
                 formal_module=None,
                 do_config_lift=True):

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.input_iterator_support = input_iterator_support
        self.output_iterator_support = output_iterator_support
        self.input_config_width = input_config_width
        self.output_config_width = output_config_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.use_sram_stub = use_sram_stub
        self.sram_macro_info = sram_macro_info
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

        self.data_words_per_set = 2 ** self.config_addr_width
        self.sets = int((self.fw_int * self.mem_depth) / self.data_words_per_set)

        self.sets_per_macro = max(1, int(self.mem_depth / self.data_words_per_set))
        self.total_sets = max(1, self.banks * self.sets_per_macro)

        # Create a MemoryTileBuilder
        MTB = MemoryTileBuilder(name, True)

        # For our current implementation, we are just using 1 bank of SRAM
        mem_banks = 1
        MTB.set_banks(mem_banks)

        # Declare and inject the memory interface for this memory into the MLB
        memory_params = {
            'mem_width': self.mem_width,
            'mem_depth': self.mem_depth
        }
        one_p_sram = [MemoryPort(MemoryPortType.READWRITE, delay=1, active_read=True)]

        sim = True
        tech_map = TSMC_Tech_Map()
        MTB.set_memory_interface(name_prefix="sram_idk",
                                 mem_params=memory_params,
                                 ports=one_p_sram,
                                 sim_macro_n=sim,
                                 tech_map=tech_map)

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
                                         config_width=self.input_config_width,
                                         agg_data_top=(self.formal_module == "agg")))
        else:
            controllers.append(StrgUBThin(data_width=self.data_width,
                                          mem_width=self.mem_width,
                                          mem_depth=self.mem_depth,
                                          input_addr_iterator_support=self.input_iterator_support,
                                          input_sched_iterator_support=self.input_iterator_support,
                                          output_addr_iterator_support=self.output_iterator_support,
                                          output_sched_iterator_support=self.output_iterator_support,
                                          interconnect_input_ports=self.interconnect_input_ports,
                                          interconnect_output_ports=self.interconnect_output_ports,
                                          config_width=self.input_config_width,
                                          read_delay=self.read_delay,
                                          rw_same_cycle=self.rw_same_cycle,
                                          gen_addr=self.gen_addr))

        # if self.fifo_mode:
        #     controllers.append(StrgFIFO(data_width=self.data_width,
        #                             banks=self.banks,
        #                             memory_width=self.mem_width,
        #                             rw_same_cycle=False,
        #                             read_delay=self.read_delay,
        #                             addr_width=self.address_width))

        controllers.append(StrgRAM(data_width=self.data_width,
                                   banks=self.banks,
                                   memory_width=self.mem_width,
                                   memory_depth=self.mem_depth,
                                   num_tiles=self.num_tiles,
                                   rw_same_cycle=self.rw_same_cycle,
                                   read_delay=self.read_delay,
                                   addr_width=16,
                                   prioritize_write=True))

        controllers.append(StencilValid())

        for ctrl in controllers:
            MTB.add_memory_controller(ctrl)
        # Finalize number of controllers (so we know how many bits to give the mode register)
        MTB.finalize_controllers()

        # Then add the config hooks...
        MTB.add_config_hooks(config_data_width=self.config_data_width,
                             config_addr_width=self.config_addr_width)

        print(MTB)

        MTB.realize_hw(clock_gate=add_clk_enable,
                       flush=add_flush,
                       mem_config=True,
                       do_lift_config=do_config_lift)

        self.dut = MTB

        return

    def get_verilog(self, filename="dut_mtb.sv", addit_passes={}):
        verilog(self.dut, filename=filename,
                optimize_if=False,
                additional_passes=addit_passes)

    def get_dut_object(self):
        return self.dut

    def supports(self, prop):
        attr = getattr(self, prop)
        if attr:
            return attr
        else:
            return False


if __name__ == "__main__":

    top = Top()
    top.get_verilog()

    # parser = argparse.ArgumentParser(description='LakeTop')
    # parser.add_argument("-f",
    #                     help="optional: will generate verilog, annotation file, and dim to strides/range mapping collateral to solve a formal problem. must provide module to solve for")

    # args = parser.parse_args()

    # need_config_lift = True

    # # normal generation
    # if args.f is None:
    #     prefix = ""
    #     lake_dut, need_config_lift, use_sram_stub, tsmc_info = get_lake_dut(mem_width=64)
    #     extract_formal_annotation(lake_dut, f"lake_top_annotation.txt", "full")
    # # optional: to add generator cuts for formal module verilog + annotations
    # else:
    #     module = args.f
    #     lake_dut, need_config_lift, use_sram_stub, tsmc_info = get_formal_module(module)
    #     prefix = f"{module}_"

    # # config lift happens in all possible cases by this point
    # assert not need_config_lift

    # sram_port_pass = change_sram_port_names(use_sram_stub=use_sram_stub, sram_macro_info=tsmc_info)
    # # generate verilog
    # verilog(lake_dut, filename=f"{prefix}lake_top_pohan.sv",
    #         optimize_if=False,
    #         additional_passes={"change sram port names": sram_port_pass})
