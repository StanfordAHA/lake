from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
import os as os


def build_simple_dual_port_demo(storage_capacity: int = 1024, data_width=16,
                           dims: int = 6, clock_count_width=16,
                           recurrence=True, reg_file=False) -> Spec:

    read_delay = 1 if reg_file else 1

    ls = Spec()

    in_port = Port(ext_data_width=data_width, runtime=Runtime.STATIC, direction=Direction.IN)
    out_port = Port(ext_data_width=data_width, runtime=Runtime.STATIC, direction=Direction.OUT)

    ls.register(in_port, out_port)

    in_id = IterationDomain(dimensionality=dims, extent_width=clock_count_width)
    in_ag = AddressGenerator(dimensionality=dims, recurrence=recurrence)
    in_sg = ScheduleGenerator(dimensionality=dims, recurrence=recurrence)

    out_id = IterationDomain(dimensionality=dims, extent_width=clock_count_width)
    out_ag = AddressGenerator(dimensionality=dims, recurrence=recurrence)
    out_sg = ScheduleGenerator(dimensionality=dims, recurrence=recurrence)

    ls.register(in_id, in_ag, in_sg)
    ls.register(out_id, out_ag, out_sg)

    # stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map)
    stg = SingleBankStorage(capacity=storage_capacity)
    wr_mem_port = MemoryPort(data_width=16, mptype=MemoryPortType.W, delay=1)
    rd_mem_port = MemoryPort(data_width=16, mptype=MemoryPortType.R, delay=read_delay)
    ls.register(stg, wr_mem_port, rd_mem_port, stg)

    # All cores are registered at this point
    # Now connect them

    # In to in
    ls.connect(in_port, in_id)
    ls.connect(in_port, in_ag)
    ls.connect(in_port, in_sg)

    # Out to out
    ls.connect(out_port, out_id)
    ls.connect(out_port, out_ag)
    ls.connect(out_port, out_sg)

    # In and Out to memory ports
    ls.connect(in_port, wr_mem_port)
    ls.connect(out_port, rd_mem_port)

    # Memory Ports to storage
    ls.connect(wr_mem_port, stg)
    ls.connect(rd_mem_port, stg)

    return ls

