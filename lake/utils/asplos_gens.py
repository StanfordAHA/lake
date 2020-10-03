from lake.top.lake_top import LakeTop
from lake.utils.sram_macro import SRAMMacroInfo
import kratos as kts
import magma as m


def gen_verilog(lake_dut, out_file, flatten=False):

    magma_dut = kts.util.to_magma(lake_dut,
                                  flatten_array=flatten,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    m.compile(out_file, magma_dut, output='coreir-verilog')


def create_bare_dual_port():
    lt_dut = LakeTop(data_width=16,  # CGRA Params
                     mem_width=16,
                     mem_depth=512,
                     banks=1,
                     input_iterator_support=6,  # Addr Controllers
                     output_iterator_support=6,
                     input_config_width=16,
                     output_config_width=16,
                     interconnect_input_ports=1,  # Connection to int
                     interconnect_output_ports=1,
                     mem_input_ports=1,
                     mem_output_ports=1,
                     use_sram_stub=True,
                     sram_macro_info=SRAMMacroInfo("tsmc_name"),
                     read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                     rw_same_cycle=True,  # Does the memory allow r+w in same cycle?
                     agg_height=4,
                     tb_sched_max=16,
                     config_data_width=32,
                     config_addr_width=8,
                     num_tiles=1,
                     remove_tb=False,
                     fifo_mode=False,
                     add_clk_enable=True,
                     add_flush=True,
                     name="LakeTop",
                     gen_addr=False)

    gen_verilog(lt_dut, "bare_dual_port")


def create_dual_port_w_ctrl():
    lt_dut = LakeTop(data_width=16,  # CGRA Params
                     mem_width=16,
                     mem_depth=512,
                     banks=1,
                     input_iterator_support=6,  # Addr Controllers
                     output_iterator_support=6,
                     input_config_width=16,
                     output_config_width=16,
                     interconnect_input_ports=1,  # Connection to int
                     interconnect_output_ports=1,
                     mem_input_ports=1,
                     mem_output_ports=1,
                     use_sram_stub=True,
                     sram_macro_info=SRAMMacroInfo("tsmc_name"),
                     read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                     rw_same_cycle=True,  # Does the memory allow r+w in same cycle?
                     agg_height=4,
                     tb_sched_max=16,
                     config_data_width=32,
                     config_addr_width=8,
                     num_tiles=1,
                     remove_tb=False,
                     fifo_mode=False,
                     add_clk_enable=True,
                     add_flush=True,
                     name="LakeTop",
                     gen_addr=True)

    gen_verilog(lt_dut, "dual_port_w_ctrl")


def create_vectorized_sram():
    lt_dut = LakeTop(data_width=16,  # CGRA Params
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
                     tb_sched_max=16,
                     config_data_width=32,
                     config_addr_width=8,
                     num_tiles=1,
                     remove_tb=False,
                     fifo_mode=True,
                     add_clk_enable=True,
                     add_flush=True,
                     name="LakeTop",
                     gen_addr=True)

    gen_verilog(lt_dut, "vectorized_single_port", flatten=True)


if __name__ == "__main__":
    # create_bare_dual_port()
    # create_dual_port_w_ctrl()
    create_vectorized_sram()
