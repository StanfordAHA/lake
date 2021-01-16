from lake.dsl.lake_imports import *

# Amber memory tile: 2 aggs -> SRAM -> 2 tbs
tile = Lake(16, 2, 2)

fw = 4  # fetch_width for SRAM
num_aggs, num_tbs = 2, 2

# wide fetch SRAM
sram_port = MemPort(1, 1)
sram_params = make_params("sram", 512, read_write_port_width=fw)
tile.add_memory(sram_params, read_write_ports=[sram_port])

# aggregators: serial to parallel converters before SRAM
for i in range(num_aggs):
    mem_name = f"agg{i}"
    write_port, read_port = MemPort(1, 1), MemPort(0, 1)
    agg_params = make_params(mem_name,
                             fw,
                             read_port_width=fw,
                             write_port_width=1)
    tile.add_memory(agg_params, [write_port], [read_port])

    tile.add_input_edge(i, mem_name)
    tile.add_edge(mem_name,
                  "sram",
                  dim=6,
                  max_range=65536)

# transpose buffers: parallel to serial converters after SRAM
for i in range(num_tbs):
    mem_name = f"tb{i}"
    write_port, read_port = MemPort(1, 1), MemPort(0, 1)
    tb_params = make_params(mem_name,
                            fw * 2,  # double buffer
                            read_port_width=1,
                            write_port_width=fw)
    tile.add_memory(tb_params, [write_port], [read_port])

    tile.add_output_edge(i, mem_name)
    tile.add_edge("sram",
                  mem_name,
                  dim=6,
                  max_range=65536)

# tile.construct_lake("memtile")
