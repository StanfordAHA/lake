from lake.top.core_combiner import CoreCombiner
from lake.spec.spec_memory_controller import SpecMemoryController
from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType, LFComparisonOperator
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ReadyValidScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.top.tech_maps import GF_Tech_Map
import argparse
import os

def build_four_port_wide_fetch_rv(storage_capacity=16384, data_width=16, dims: int = 6, vec_width=4, physical=False,
                                  reg_file=False, vec_capacity=2, opt_rv=True) -> Spec:

    # a reg file can't be used to build this...

    ls = Spec(opt_rv=opt_rv)

    in_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                   vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.IN,
                   opt_rv=opt_rv)
    in_port2 = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                    vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.IN,
                    opt_rv=opt_rv)
    out_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                    vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.OUT,
                    opt_rv=opt_rv)
    out_port2 = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                     vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.OUT,
                     opt_rv=opt_rv)

    ls.register(in_port, in_port2, out_port, out_port2)

    in_id = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag = AddressGenerator(dimensionality=dims)
    in_sg = ReadyValidScheduleGenerator(dimensionality=dims)

    in_id2 = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag2 = AddressGenerator(dimensionality=dims)
    in_sg2 = ReadyValidScheduleGenerator(dimensionality=dims)

    out_id = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag = AddressGenerator(dimensionality=dims)
    out_sg = ReadyValidScheduleGenerator(dimensionality=dims)

    out_id2 = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag2 = AddressGenerator(dimensionality=dims)
    out_sg2 = ReadyValidScheduleGenerator(dimensionality=dims)

    ls.register(in_id, in_ag, in_sg)
    ls.register(in_id2, in_ag2, in_sg2)
    ls.register(out_id, out_ag, out_sg)
    ls.register(out_id2, out_ag2, out_sg2)

    data_bytes = (data_width * vec_width) // 8
    tech_map = None
    if physical:
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes, width=data_width * vec_width, dual_port=False)

    # 1024 Bytes
    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map)
    shared_rw_mem_port = MemoryPort(data_width=data_width * vec_width, mptype=MemoryPortType.RW, delay=1)
    ls.register(stg, shared_rw_mem_port)

    # All cores are registered at this point
    # Now connect them

    # In to in
    ls.connect(in_port, in_id)
    ls.connect(in_port, in_ag)
    ls.connect(in_port, in_sg)

    ls.connect(in_port2, in_id2)
    ls.connect(in_port2, in_ag2)
    ls.connect(in_port2, in_sg2)

    # Out to out
    ls.connect(out_port, out_id)
    ls.connect(out_port, out_ag)
    ls.connect(out_port, out_sg)

    ls.connect(out_port2, out_id2)
    ls.connect(out_port2, out_ag2)
    ls.connect(out_port2, out_sg2)

    # In and Out to shared memory port
    ls.connect(in_port, shared_rw_mem_port)
    ls.connect(in_port2, shared_rw_mem_port)
    ls.connect(out_port, shared_rw_mem_port)
    ls.connect(out_port2, shared_rw_mem_port)

    # Memory Ports to storage
    ls.connect(shared_rw_mem_port, stg)

    return ls

if __name__ == "__main__":

    # get the spec
    spec = build_four_port_wide_fetch_rv()
    # Instantiate the core
    controllers = [SpecMemoryController(spec=spec)]

    core_combiner = CoreCombiner(controllers=controllers,
                                 name="example_core_combiner",)

    # Run the core
    # core_combiner.run()