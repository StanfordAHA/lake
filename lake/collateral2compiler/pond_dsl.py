from lake.collateral2compiler.lake_imports import *

pond = TopLake(16, 1, 1)

read_port = MemPort(0, 0)
write_port = MemPort(1, 0)

pond_params = {"name": "pond",
               "capacity": 32,
               "word_width": 16,
               "read_port_width": 1,
               "write_port_width": 1}

pond.add_memory(pond_params, read_ports=[read_port], write_ports=[write_port])

# for both compiler collateral and HW generation
pond.construct_lake()
