from lake.collateral2compiler.top_lake import TopLake
from lake.collateral2compiler.mem_port import MemPort

# example of DSL

# mem_collateral is part of Lake, not exposed to user
# could consumer ports and parameters and then set port_info appropriately (see second example)

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

'''tb_write_port = MemPort(1, 0)
tb_read_port = MemPort(0, 0)

tb_params = {"name": "tb",
             "capacity": 8,
             "word_width": 16,
             "read_port_width": 1,
             "write_port_width": 1,
             "chaining": 0}

tile.add_memory(tb_params, [tb_write_port], [tb_read_port])

tile.add_edge(from_signal="agg", 
              to_signal="tb",
              addr_gen_dim=4,
              addr_gen_max_range=65536,
              addr_gen_max_stride=65536)

tile.construct_lake()
