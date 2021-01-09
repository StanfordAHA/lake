from lake.dsl.lake_imports import *
from frail.ast import *

# example of DSL (makes current mem tile with 2 agg,
# wide SRAM, 2 tb

# LAKE OBJECT MUST BE FIRST INSTANTIATED
# IMPORTANT: PORTS MUST BE INSTANTIATED BEFORE MEMORIES
# MEMORIES SHOULD BE INSTANTIATED BEFORE EDGES

# addressor in frail

# configuration registers
x_max = var_f("x_max")
y_max = var_f("y_max")
x_stride = var_f("x_stride")
y_stride = var_f("y_stride")
offset = var_f("offset")

# original addressor design
def create_og_design():
    x_unit_counter = scan_const_f(lambda z: if_f(eq_f(z, sub_f(x_max, int_f(1))), int_f(0), add_f(z, int_f(1))))
    y_unit_counter = scan_const_f(lambda z: if_f(eq_f(x_unit_counter.get_seq(), sub_f(x_max, int_f(1))), add_f(z, int_f(1)), z))
    x_counter = scan_const_f(lambda z: if_f(eq_f(x_unit_counter.get_seq(), sub_f(x_max, int_f(1))), int_f(0), add_f(z, x_stride)))
    y_counter = scan_const_f(lambda z: if_f(eq_f(x_unit_counter.get_seq(), sub_f(x_max, int_f(1))), add_f(z, y_stride), z))
    og_design = scan_const_f(lambda z: add_f(add_f(x_counter.get_seq(), y_counter.get_seq()), offset))
    return og_design

og_design = create_og_design()

# word_width, input_ports, output_ports
tile = Lake(16, 2, 2)

tile.set_addressor(og_design, "og_design")

# MemPort attributes are latency, initiation interval
agg_write_port = MemPort(1, 0)
agg_read_port = MemPort(0, 0)
agg_params = make_params("agg", 4, read_port_width=4, write_port_width=1)
tile.add_memory(agg_params, [agg_write_port], [agg_read_port])
tile.add_input_edge(port=0, mem_name="agg")

agg1_write_port = MemPort(1, 0)
agg1_read_port = MemPort(0, 0)
agg1_params = make_params("agg1", 4, read_port_width=4, write_port_width=1)
tile.add_memory(agg1_params, [agg1_write_port], [agg1_read_port])
tile.add_input_edge(1, "agg1")

sram_write_read_port = MemPort(1, 0)
sram_params = make_params("sram", 512, read_write_port_width=4)
# , use_macro=True, macro_name="SRAM_example_name")
# , num_chain=2)
tile.add_memory(sram_params, read_write_ports=[sram_write_read_port])

tile.add_edge("agg", "sram")
tile.add_edge("agg1", "sram")

tb_write_port = MemPort(1, 0)
tb_read_port = MemPort(0, 0)
tb_params = make_params("tb", 8, read_port_width=1, write_port_width=4)
tile.add_memory(tb_params, [tb_write_port], [tb_read_port])
tile.add_output_edge(0, "tb")

tb1_write_port = MemPort(1, 0)
tb1_read_port = MemPort(0, 0)
tb1_params = make_params("tb1", 8, read_port_width=1, write_port_width=4)
tile.add_memory(tb1_params, [tb1_write_port], [tb1_read_port])
tile.add_output_edge(1, "tb1")

tile.add_edge("sram", "tb")
tile.add_edge("sram", "tb1")

# for both compiler collateral and HW generation
tile.construct_lake("memtile.sv")
