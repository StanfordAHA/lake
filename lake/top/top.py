from lake.modules.stencil_valid import StencilValid
from lake.top.tech_maps import TSMC_Tech_Map
from lake.top.memory_interface import MemoryPort, MemoryPortType
from lake.attributes.formal_attr import *
from kratos import *
from lake.modules.passthru import *
from lake.modules.strg_ub_vec import StrgUBVec
from lake.modules.strg_ub_thin import StrgUBThin
from lake.modules.strg_fifo import StrgFIFO
from lake.modules.strg_RAM import StrgRAM
from lake.top.memtile_builder import MemoryTileBuilder


class Top():
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
                 use_sim_sram=True,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 config_data_width=32,
                 config_addr_width=8,
                 num_tiles=1,
                 add_clk_enable=True,
                 add_flush=True,
                 name="mek",
                 gen_addr=True,
                 stencil_valid=True,
                 formal_module=None,
                 do_config_lift=True,
                 fifo_mode=False):

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.input_iterator_support = input_iterator_support
        self.output_iterator_support = output_iterator_support
        self.config_width = config_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.use_sim_sram = use_sim_sram
        self.agg_height = 4
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

        # Create a MemoryTileBuilder
        MTB = MemoryTileBuilder(name, True)

        # For our current implementation, we are just using 1 bank of SRAM
        MTB.set_banks(self.banks)

        # Declare and inject the memory interface for this memory into the MLB
        memory_params = {
            'mem_width': self.mem_width,
            'mem_depth': self.mem_depth
        }
        one_p_sram = [MemoryPort(MemoryPortType.READWRITE, delay=1, active_read=True)]

        tech_map = TSMC_Tech_Map()
        MTB.set_memory_interface(name_prefix="sram_idk",
                                 mem_params=memory_params,
                                 ports=one_p_sram,
                                 sim_macro_n=self.use_sim_sram,
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
                                         config_width=self.config_width,
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
                                          config_width=self.config_width,
                                          read_delay=self.read_delay,
                                          rw_same_cycle=self.rw_same_cycle,
                                          gen_addr=self.gen_addr))

        if self.fifo_mode:
            controllers.append(StrgFIFO(data_width=self.data_width,
                                        banks=self.banks,
                                        memory_width=self.mem_width,
                                        rw_same_cycle=False,
                                        read_delay=self.read_delay,
                                        addr_width=self.data_width))

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

    def __str__(self):
        return str(self.dut)


if __name__ == "__main__":

    top = Top()
    print(top)
    top.get_verilog()
