from lake.collateral2compiler.top_lake import TopLake
from lake.collateral2compiler.mem_port import MemPort

# example of DSL (makes current mem tile with 2 agg,
# wide SRAM, 2 tb

# mem_collateral is part of TopLake for collateral
# to compiler, not exposed to user

# LAKE OBJECT MUST BE FIRST INSTANTIATED
# IMPORTANT: PORTS MUST BE INSTANTIATED BEFORE MEMORIES
# MEMORIES SHOULD BE INSTANTIATED BEFORE EDGES

# word_width, input_ports, output_ports
tile = TopLake(16, 2, 2)

# MemPort attributes are latency, initiation interval
agg_write_port = MemPort(1, 0)
agg_read_port = MemPort(0, 0)

agg_params = {"name": "agg",
              "capacity": 4,
              "word_width": 16,
              "read_port_width": 4,
              "write_port_width": 1}

# add_memory takes in params, W ports, R ports, R/W ports
tile.add_memory(agg_params, [agg_write_port], [agg_read_port])

agg1_write_port = MemPort(1, 0)
agg1_read_port = MemPort(0, 0)
agg1_params = {"name": "agg1",
               "capacity": 4,
               "word_width": 16,
               "read_port_width": 4,
               "write_port_width": 1}

tile.add_memory(agg1_params, [agg1_write_port], [agg1_read_port])

sram_write_read_port = MemPort(1, 0)

sram_params = {"name": "sram",
               "capacity": 512,
               "word_width": 16,
               "read_write_port_width": 4}

tile.add_memory(sram_params, read_write_ports=[sram_write_read_port])

tile.add_edge({"from_signal": "agg",
               "to_signal": "sram",
               # these are defaults, so not specified for further edges
               "dim": 6,
               "max_range": 65535,
               "max_stride": 65535})

tile.add_edge({"from_signal": "agg1",
               "to_signal": "sram"})

tb_write_port = MemPort(1, 0)
tb_read_port = MemPort(0, 0)

tb_params = {"name": "tb",
             "capacity": 8,
             "word_width": 16,
             "read_port_width": 1,
             "write_port_width": 1}

tile.add_memory(tb_params, [tb_write_port], [tb_read_port])

tb1_write_port = MemPort(1, 0)
tb1_read_port = MemPort(0, 0)

tb1_params = {"name": "tb1",
              "capacity": 8,
              "word_width": 16,
              "read_port_width": 1,
              "write_port_width": 1}

tile.add_memory(tb1_params, [tb1_write_port], [tb1_read_port])

tile.add_edge({"from_signal": "sram",
               "to_signal": "tb"})

tile.add_edge({"from_signal": "sram",
               "to_signal": "tb1"})

# for both compiler collateral and HW generation
tile.construct_lake()
