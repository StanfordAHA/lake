from lake.collateral2compiler.lake_imports import *

# weight buffer
weights = TopLake(64, 1, 1)

port = MemPort(1, 0)

weights_params = make_params("weights", 512, 1, 1)
weights.add_memory(weights_params, read_write_ports=[port])

weights.construct_lake("simba_weights.sv")

# accumulation buffer
accum = TopLake(8*24, 1, 1)

port = MemPort(1, 0)

accum_params = make_params("accum", 64, 1, 1)
accum.add_memory(accum_params, read_write_ports=[port])

accum.construct_lake("simba_accum.sv")

# input activation buffer
input_buffer = TopLake(64, 1, 1)

port = MemPort(1, 0)

input_params = make_params("input_buffer", 1024, 1, 1)
input_buffer.add_memory(weights_params, read_write_ports[port])

input_buffer.construct_lake("simba_input.sv")
