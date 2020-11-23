from lake.collateral2compiler.lake_imports import *

pond = TopLake(16, 1, 1)

read_port = MemPort(0, 0)
write_port = MemPort(1, 0)

pond_params = make_params("pond", 32, 1, 1)
pond.add_memory(pond_params, read_ports=[read_port], write_ports=[write_port], is_input=True, input_edge_params=[2, 2**16, 2**5], is_output=True, output_edge_params=[2, 2**16, 2**5])

pond.construct_lake()
