from lake.top.tech_maps import TSMC_Tech_Map
from lake.top.memory_interface import MemoryInterface, MemoryPort, MemoryPortType
from lake.attributes.formal_attr import *
import os
from kratos import *
from lake.modules.passthru import *
from lake.modules.sram import SRAM
from lake.modules.strg_ub_vec import StrgUBVec
from lake.modules.strg_ub_thin import StrgUBThin
from lake.modules.storage_config_seq import StorageConfigSeq
from lake.modules.register_file import RegisterFile
from lake.modules.strg_fifo import StrgFIFO
from lake.modules.strg_RAM import StrgRAM
from lake.modules.chain_accessor import ChainAccessor
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.passes.passes import lift_config_reg, change_sram_port_names
from lake.passes.cut_generator import cut_generator
from lake.utils.sram_macro import SRAMMacroInfo
from lake.utils.util import trim_config_list, extract_formal_annotation
from lake.utils.parse_clkwork_config import map_controller, extract_controller
from lake.utils.parse_clkwork_config import extract_controller_json
from lake.modules.for_loop import ForLoop
from lake.modules.spec.sched_gen import SchedGen
import kratos as kts
from _kratos import create_wrapper_flatten
import argparse
from lake.top.memtile_builder import MemoryTileBuilder


class Top():
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=32,
                 mem_depth=256,
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
                 rw_same_cycle=True,  # Does the memory allow r+w in same cycle?
                 agg_height=4,
                 config_data_width=32,
                 config_addr_width=8,
                 num_tiles=1,
                 fifo_mode=False,
                 add_clk_enable=True,
                 add_flush=True,
                 name="LakeTop",
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
        MTB = MemoryTileBuilder("laketop_mtb", True)

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

        for ctrl in controllers:
            MTB.add_memory_controller(ctrl)
        # Finalize number of controllers (so we know how many bits to give the mode register)
        MTB.finalize_controllers()

        # Then add the config hooks...
        MTB.add_config_hooks(config_data_width=self.config_data_width,
                             config_addr_width=self.config_addr_width)

        print(MTB)

        MTB.realize_hw(clock_gate=True, flush=True, mem_config=True)

        verilog(MTB, filename="top_mtb.sv",
                optimize_if=False,
                additional_passes={"lift config regs": lift_config_reg})

        return

        ########################
        ##### CLOCK ENABLE #####
        ########################
        if add_clk_enable:
            # self.clock_en("clk_en")
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            # Add input attr and formal attr...
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))
            clk_en_port.add_attribute(FormalAttr(clk_en_port.name, FormalSignalConstraint.SET1))
        if add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

        if do_config_lift:
            lift_config_reg(self.internal_generator)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def cycle_count_inc(self):
        if ~self._rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1

    def supports(self, prop):
        attr = getattr(self, prop)
        if attr:
            return attr
        else:
            return False

    def get_static_bitstream_json(self,
                                  root_node):
        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        input_ports = 1
        output_ports = 1

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Store all configurations here
        config = []

        # Compiler tells us to turn on the chain enable...
        if "chain_en" in root_node:
            config.append(("chain_chain_en", 1))

        if "in2agg_0" in root_node:
            in2agg_0 = map_controller(extract_controller_json(root_node["in2agg_0"]), "in2agg_0")
            config.append(("strg_ub_agg_only_agg_write_addr_gen_0_starting_addr", in2agg_0.in_data_strt))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_0_enable", 1))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_starting_addr", in2agg_0.cyc_strt))
            config.append(("strg_ub_agg_only_loops_in2buf_0_dimensionality", in2agg_0.dim))
            for i in range(in2agg_0.dim):
                config.append((f"strg_ub_agg_only_loops_in2buf_0_ranges_{i}", in2agg_0.extent[i]))
                config.append((f"strg_ub_agg_only_agg_write_addr_gen_0_strides_{i}", in2agg_0.in_data_stride[i]))
                config.append((f"strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_{i}", in2agg_0.cyc_stride[i]))

        if "in2agg_1" in root_node:
            in2agg_1 = map_controller(extract_controller_json(root_node["in2agg_1"]), "in2agg_1")
            config.append(("strg_ub_agg_only_agg_write_addr_gen_1_starting_addr", in2agg_1.in_data_strt))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_1_enable", 1))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_starting_addr", in2agg_1.cyc_strt))
            config.append(("strg_ub_agg_only_loops_in2buf_1_dimensionality", in2agg_1.dim))
            for i in range(in2agg_1.dim):
                config.append((f"strg_ub_agg_only_loops_in2buf_1_ranges_{i}", in2agg_1.extent[i]))
                config.append((f"strg_ub_agg_only_agg_write_addr_gen_1_strides_{i}", in2agg_1.in_data_stride[i]))
                config.append((f"strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_{i}", in2agg_1.cyc_stride[i]))

        if "agg2sram_0" in root_node:
            agg2sram_0 = map_controller(extract_controller_json(root_node["agg2sram_0"]), "agg2sram_0")
            config.append(("strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_dimensionality", agg2sram_0.dim))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_0_enable", 1))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_starting_addr", agg2sram_0.cyc_strt))
            config.append(("strg_ub_agg_only_agg_read_addr_gen_0_starting_addr", agg2sram_0.out_data_strt))
            config.append(("strg_ub_sram_only_input_addr_gen_0_starting_addr", agg2sram_0.in_data_strt))
            for i in range(agg2sram_0.dim):
                config.append((f"strg_ub_agg_only_agg_read_addr_gen_0_strides_{i}", agg2sram_0.out_data_stride[i]))
                config.append((f"strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_{i}", agg2sram_0.extent[i]))
                config.append((f"strg_ub_sram_only_input_addr_gen_0_strides_{i}", agg2sram_0.in_data_stride[i]))
                config.append((f"strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_{i}", agg2sram_0.cyc_stride[i]))

        if "agg2sram_1" in root_node:
            agg2sram_1 = map_controller(extract_controller_json(root_node["agg2sram_1"]), "agg2sram_1")
            config.append(("strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_dimensionality", agg2sram_1.dim))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_1_enable", 1))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_starting_addr", agg2sram_1.cyc_strt))
            config.append(("strg_ub_agg_only_agg_read_addr_gen_1_starting_addr", agg2sram_1.out_data_strt))
            config.append(("strg_ub_sram_only_input_addr_gen_1_starting_addr", agg2sram_1.in_data_strt))
            for i in range(agg2sram_1.dim):
                config.append((f"strg_ub_agg_only_agg_read_addr_gen_1_strides_{i}", agg2sram_1.out_data_stride[i]))
                config.append((f"strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_{i}", agg2sram_1.extent[i]))
                config.append((f"strg_ub_sram_only_input_addr_gen_1_strides_{i}", agg2sram_1.in_data_stride[i]))
                config.append((f"strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_{i}", agg2sram_1.cyc_stride[i]))

        # Count tbs
        num_tbs = 0

        if "tb2out_0" in root_node:
            num_tbs += 1
            tb2out_0 = map_controller(extract_controller_json(root_node["tb2out_0"]), "tb2out_0")
            config.append(("strg_ub_tb_only_tb_read_sched_gen_0_enable", 1))
            config.append(("strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_starting_addr", tb2out_0.cyc_strt))
            config.append(("strg_ub_tb_only_tb_read_addr_gen_0_starting_addr", tb2out_0.out_data_strt))
            config.append(("strg_ub_tb_only_loops_buf2out_read_0_dimensionality", tb2out_0.dim))
            for i in range(tb2out_0.dim):
                config.append((f"strg_ub_tb_only_loops_buf2out_read_0_ranges_{i}", tb2out_0.extent[i]))
                config.append((f"strg_ub_tb_only_tb_read_addr_gen_0_strides_{i}", tb2out_0.out_data_stride[i]))
                config.append((f"strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_{i}", tb2out_0.cyc_stride[i]))

        if "tb2out_1" in root_node:
            num_tbs += 1
            tb2out_1 = map_controller(extract_controller_json(root_node["tb2out_1"]), "tb2out_1")
            config.append(("strg_ub_tb_only_tb_read_sched_gen_1_enable", 1))
            config.append(("strg_ub_tb_only_tb_read_addr_gen_1_starting_addr", tb2out_1.out_data_strt))
            config.append(("strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_starting_addr", tb2out_1.cyc_strt))
            config.append(("strg_ub_tb_only_loops_buf2out_read_1_dimensionality", tb2out_1.dim))
            for i in range(tb2out_1.dim):
                config.append((f"strg_ub_tb_only_loops_buf2out_read_1_ranges_{i}", tb2out_1.extent[i]))
                config.append((f"strg_ub_tb_only_tb_read_addr_gen_1_strides_{i}", tb2out_1.out_data_stride[i]))
                config.append((f"strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_{i}", tb2out_1.cyc_stride[i]))

        if "sram2tb_0" in root_node:
            sram2tb_0 = map_controller(extract_controller_json(root_node["sram2tb_0"]), "sram2tb_0")
            config.append(("strg_ub_sram_only_output_addr_gen_0_starting_addr", sram2tb_0.out_data_strt))
            config.append(("strg_ub_tb_only_tb_write_addr_gen_0_starting_addr", sram2tb_0.in_data_strt))
            config.append(("strg_ub_sram_tb_shared_output_sched_gen_0_enable", 1))
            config.append(("strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_starting_addr", sram2tb_0.cyc_strt))
            config.append(("strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_dimensionality", sram2tb_0.dim))
            for i in range(sram2tb_0.dim):
                config.append((f"strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_{i}", sram2tb_0.extent[i]))
                config.append((f"strg_ub_sram_only_output_addr_gen_0_strides_{i}", sram2tb_0.out_data_stride[i]))
                config.append((f"strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_{i}", sram2tb_0.cyc_stride[i]))
                config.append((f"strg_ub_tb_only_tb_write_addr_gen_0_strides_{i}", sram2tb_0.in_data_stride[i]))

        if "sram2tb_1" in root_node:
            sram2tb_1 = map_controller(extract_controller_json(root_node["sram2tb_1"]), "sram2tb_1")
            config.append(("strg_ub_sram_only_output_addr_gen_1_starting_addr", sram2tb_1.out_data_strt))
            config.append(("strg_ub_tb_only_tb_write_addr_gen_1_starting_addr", sram2tb_1.in_data_strt))
            config.append(("strg_ub_sram_tb_shared_output_sched_gen_1_enable", 1))
            config.append(("strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_starting_addr", sram2tb_1.cyc_strt))
            config.append(("strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_dimensionality", sram2tb_1.dim))
            for i in range(sram2tb_1.dim):
                config.append((f"strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_{i}", sram2tb_1.extent[i]))
                config.append((f"strg_ub_sram_only_output_addr_gen_1_strides_{i}", sram2tb_1.out_data_stride[i]))
                config.append((f"strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_{i}", sram2tb_1.cyc_stride[i]))
                config.append((f"strg_ub_tb_only_tb_write_addr_gen_1_strides_{i}", sram2tb_1.in_data_stride[i]))

        if "stencil_valid" in root_node:
            stencil_valid = map_controller(extract_controller_json(root_node["stencil_valid"]), "stencil_valid")
            # Check actual stencil valid property of hardware before programming
            if self.stencil_valid:
                config.append((f"stencil_valid_sched_gen_enable", 1))
                config.append((f"stencil_valid_sched_gen_sched_addr_gen_starting_addr", stencil_valid.cyc_strt))
                config.append((f"loops_stencil_valid_dimensionality", stencil_valid.dim))
                for i in range(stencil_valid.dim):
                    config.append((f"loops_stencil_valid_ranges_{i}", stencil_valid.extent[i]))
                    config.append((f"stencil_valid_sched_gen_sched_addr_gen_strides_{i}", stencil_valid.cyc_stride[i]))

        # Control Signals...
        # Set the mode and activate the tile...
        config.append(("flush_reg_sel", 0))  # 1
        config.append(("flush_reg_value", 0))  # 1
        config.append(("mode", 0))  # 2
        config.append(("tile_en", 1))  # 1

        # TODO: Maybe need to check if size 1?
        for i in range(input_ports):
            config.append((f"ren_in_{i}_reg_sel", 1))
            config.append((f"ren_in_{i}_reg_value", 0))

        for i in range(output_ports):
            config.append((f"wen_in_{i}_reg_sel", 1))
            config.append((f"wen_in_{i}_reg_value", 0))

        return trim_config_list(flattened, config)

    def get_static_bitstream(self,
                             config_path,
                             in_file_name="",
                             out_file_name=""):

        # Getting bitstreams is a little unwieldy due to fault (or its underlying implementation) not
        # handling arrays in the interface.
        # To alleviate this, we create the flattened wrapper so we can query widths of config
        # registers and trim values to their bitwidths...
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        config = []

        # set the mode and activate the tile
        config.append(("mode", 0))
        config.append(("tile_en", 1))

        # Check the hardware if it supports stencil valid
        if self.stencil_valid:
            cfg_path = config_path + '/' + 'stencil_valid.csv'
            # Check if the stencil valid file exists...if it doesn't we just won't program it
            if os.path.exists(cfg_path):
                stcl_valid = map_controller(extract_controller(cfg_path), "stencil_valid")
                config.append((f"loops_stencil_valid_dimensionality", stcl_valid.dim))
                config.append((f"stencil_valid_sched_gen_enable", 1))
                config.append((f"stencil_valid_sched_gen_sched_addr_gen_starting_addr", stcl_valid.cyc_strt))
                for i in range(stcl_valid.dim):
                    config.append((f"loops_stencil_valid_ranges_{i}", stcl_valid.extent[i]))
                    config.append((f"stencil_valid_sched_gen_sched_addr_gen_strides_{i}", stcl_valid.cyc_stride[i]))
            else:
                print("No configuration file provided for stencil valid...are you expecting one to exist?")
                print(f"Bogus stencil valid path: {cfg_path}")

        # Refactored STRG_UB config into the respective options
        config += self.strg_ub.get_static_bitstream(config_path=config_path,
                                                    in_file_name=in_file_name,
                                                    out_file_name=out_file_name)

        return trim_config_list(flattened, config)


# formal module functions
def get_formal_module(module):
    lake_dut, need_config_lift, use_sram_stub, tsmc_info = \
        get_lake_dut(module,
                     # need to lift config regs after generator cuts
                     do_config_lift=False)

    # cuts for modular formal solving
    if module == "agg":
        cut_generator(lake_dut["strg_ub"]["sram_only"])
        cut_generator(lake_dut["strg_ub"]["sram_tb_shared"])
        cut_generator(lake_dut["strg_ub"]["tb_only"])
    elif module == "sram":
        cut_generator(lake_dut["strg_ub"]["agg_only"])
        cut_generator(lake_dut["strg_ub"]["tb_only"])
    elif module == "tb":
        cut_generator(lake_dut["strg_ub"]["agg_only"])
        cut_generator(lake_dut["strg_ub"]["agg_sram_shared"])
        cut_generator(lake_dut["strg_ub"]["sram_only"])
    else:
        print("Error! Invalid module name given...must be one of agg, sram, or tb. Cuts not performed.")
        return lake_dut, need_config_lift, use_sram_stub, tsmc_info

    # config regs pass (needs to be after generator cuts)
    lift_config_reg(lake_dut.internal_generator)
    need_config_lift = False

    # extract formal annotation after config regs have been lifted up
    extract_formal_annotation(lake_dut, f"{module}_lake_top_annotation.txt", module)

    return lake_dut, need_config_lift, use_sram_stub, tsmc_info

    print(f"Supports Stencil Valid: {lake_dut.supports('stencil_valid')}")

    # if do_config_lift, then do not need_config_lift later
    return lake_dut, not do_config_lift, use_sram_stub, tsmc_info


if __name__ == "__main__":

    top = Top()

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
