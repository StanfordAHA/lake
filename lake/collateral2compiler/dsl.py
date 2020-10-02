from lake.collateral2compiler.top_lake import TopLake
from lake.collateral2compiler.mem_port import MemPort

# example of DSL

# mem_collateral is part of Lake, not exposed to user

# LAKE OBJECT MUST BE FIRST INSTANTIATED
# IMPORTANT: PORTS MUST BE INSTANTIATED BEFORE MEMORIES
# MEMORIES MUST BE INSTANTIATED BEFORE PORTS

tile = TopLake()

agg_write_port = MemPort(1, 0)
agg_read_port = MemPort(0, 0)

agg_params = {"name": "agg",
              "capacity": 4,
              "word_width": 16,
              "read_port_width": 4,
              "write_port_width": 1,
              "chaining": 0}

tile.add_memory(agg_params, [agg_write_port], [agg_read_port])


agg1_write_port = MemPort(1, 0)
agg1_read_port = MemPort(0, 0)
agg1_params = {"name": "agg1",
               "capacity": 4,
               "word_width": 16,
               "read_port_width": 4,
               "write_port_width": 1,
               "chaining": 0}

tile.add_memory(agg1_params, [agg1_write_port], [agg1_read_port])

sram_write_read_port = MemPort(1, 0)

sram_params = {"name": "sram",
               "capacity": 512,
               "word_width": 16,
               "read_write_port_width": 4,
               "chaining": 1}

tile.add_memory(sram_params, read_write_ports=[sram_write_read_port])

tile.add_edge({"from_signal": "agg",
               "to_signal": "sram"})

tile.add_edge({"from_signal": "agg1",
               "to_signal": "sram"})

tb_write_port = MemPort(1, 0)
tb_read_port = MemPort(0, 0)

tb_params = {"name": "tb",
             "capacity": 8,
             "word_width": 16,
             "read_port_width": 1,
             "write_port_width": 1,
             "chaining": 0}

tile.add_memory(tb_params, [tb_write_port], [tb_read_port])

tb1_write_port = MemPort(1, 0)
tb1_read_port = MemPort(0, 0)

tb1_params = {"name": "tb1",
              "capacity": 8,
              "word_width": 16,
              "read_port_width": 1,
              "write_port_width": 1,
              "chaining": 0}

tile.add_memory(tb1_params, [tb1_write_port], [tb1_read_port])

tile.add_edge({"from_signal": "sram",
               "to_signal": "tb"})

tile.add_edge({"from_signal": "sram",
               "to_signal": "tb1"})

tile.construct_lake()

'''tile.add_edge(from_signal="agg",
             to_signal="tb",
              addr_gen_dim=4,
              addr_gen_max_range=65536,
              addr_gen_max_stride=65536)'''
