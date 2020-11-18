from lake.collateral2compiler.lake_imports import *

pond = TopLake(16, 1, 1)

read_port = MemPort(0, 0)
write_port = MemPort(1, 0)

pond_params = make_params("pond", 32, 1, 1)
pond.add_memory(pond_params, read_ports=[read_port], write_ports=[write_port])

# pond.construct_lake()
