from lake.dsl.lake_imports import *

tile = Lake(16, 2, 2)

fw = 4  # fetch_width for SRAM

# wide fetch SRAM
sram_port = MemPort(1, 1)
sram_params = make_params("sram", 512, read_write_port_width=fw)
tile.add_memory(sram_params, read_write_ports=[sram_port])

# aggregators: serial to parallel converters before SRAM
for i in range(2):
    mem_name = f"agg{i}"
    if i == 0:
        mem_name = "agg"
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
for i in range(2):
    mem_name = f"tb{i}"
    if i == 0:
        mem_name = "tb"
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
