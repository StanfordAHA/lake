from lake.dsl.lake_imports import *
from frail.ast import *

# example of DSL (makes current mem tile with 2 agg,
# wide SRAM, 2 tb

# LAKE OBJECT MUST BE FIRST INSTANTIATED
# IMPORTANT: PORTS MUST BE INSTANTIATED BEFORE MEMORIES
# MEMORIES SHOULD BE INSTANTIATED BEFORE EDGES

# addressor in frail
# original addressor design


def create_og_design6():

    # configuration registers
    r0, r1, r2, r3, r4, r5 = var_f("range_0"), var_f("range_1"), var_f("range_2"), var_f("range_3"), var_f("range_4"), var_f("range_5")
    s0, s1, s2, s3, s4, s5 = var_f("stride_0"), var_f("stride_1"), var_f("stride_2"), var_f("stride_3"), var_f("stride_4"), var_f("stride_5")
    offset = var_f("offset")

    uc0 = scan_const_f(lambda z: if_f(eq_f(z, sub_f(r0, int_f(1))), int_f(0), add_f(z, int_f(1))))
    uc1 = scan_const_f(lambda z: if_f(eq_f(uc0.get_seq(), sub_f(r0, int_f(1))), add_f(z, int_f(1)), z))
    uc2 = scan_const_f(lambda z: if_f(eq_f(uc1.get_seq(), sub_f(r1, int_f(1))), add_f(z, int_f(1)), z))
    uc3 = scan_const_f(lambda z: if_f(eq_f(uc2.get_seq(), sub_f(r2, int_f(1))), add_f(z, int_f(1)), z))
    uc4 = scan_const_f(lambda z: if_f(eq_f(uc3.get_seq(), sub_f(r3, int_f(1))), add_f(z, int_f(1)), z))
    uc5 = scan_const_f(lambda z: if_f(eq_f(uc4.get_seq(), sub_f(r4, int_f(1))), add_f(z, int_f(1)), z))

    c0 = scan_const_f(lambda z: if_f(eq_f(uc0.get_seq(), sub_f(r0, int_f(1))), int_f(0), add_f(z, s0)))
    c1 = scan_const_f(lambda z: if_f(eq_f(uc0.get_seq(), sub_f(r0, int_f(1))), add_f(z, s1), z))
    c2 = scan_const_f(lambda z: if_f(eq_f(uc1.get_seq(), sub_f(r1, int_f(1))), add_f(z, s2), z))
    c3 = scan_const_f(lambda z: if_f(eq_f(uc2.get_seq(), sub_f(r2, int_f(1))), add_f(z, s3), z))
    c4 = scan_const_f(lambda z: if_f(eq_f(uc3.get_seq(), sub_f(r3, int_f(1))), add_f(z, s4), z))
    c5 = scan_const_f(lambda z: if_f(eq_f(uc4.get_seq(), sub_f(r4, int_f(1))), add_f(z, s5), z))

    return scan_const_f(lambda z: add_f(offset, add_f(c0.get_seq(), add_f(c1.get_seq(), add_f(c2.get_seq(), add_f(c3.get_seq(), add_f(c4.get_seq(), c5.get_seq())))))))


og_design6 = create_og_design6()

# word_width, input_ports, output_ports
tile = Lake(16, 2, 2)

tile.set_addressor(og_design6, "og_design6")

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
