from lake.dsl.lake_imports import *

pond = Lake(16, 1, 1)

read_port = MemPort(0, 1)
write_port = MemPort(1, 1)

pond_params = make_params("pond", 32, 1, 1)
pond.add_memory(pond_params, read_ports=[read_port], write_ports=[write_port])
pond.add_input_edge(0, "pond", dim=2, max_stride=2**5)
pond.add_output_edge(0, "pond", dim=2, max_stride=2**5)

# pond.construct_lake("pond")
