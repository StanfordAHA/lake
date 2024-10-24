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
                           dims: int = 6, recurrence=True) -> Spec:

    # 1. Initialize the spec...
    ls = Spec()

    # 2. Define Ports (and register them)
    in_port = Port(ext_data_width=data_width, runtime=Runtime.STATIC, direction=Direction.IN)
    out_port = Port(ext_data_width=data_width, runtime=Runtime.STATIC, direction=Direction.OUT)
    ls.register(in_port, out_port)

    # 3. Define ID, AG, SG for each Port (and register them)
    in_id = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag = AddressGenerator(dimensionality=dims, recurrence=recurrence)
    in_sg = ScheduleGenerator(dimensionality=dims, recurrence=recurrence)
    out_id = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag = AddressGenerator(dimensionality=dims, recurrence=recurrence)
    out_sg = ScheduleGenerator(dimensionality=dims, recurrence=recurrence)
    ls.register(in_id, in_ag, in_sg)
    ls.register(out_id, out_ag, out_sg)

    # 4. Define the Storage and its MemoryPort loadout (and register them)
    stg = SingleBankStorage(capacity=storage_capacity)
    wr_mem_port = MemoryPort(data_width=data_width, mptype=MemoryPortType.W, delay=1)
    rd_mem_port = MemoryPort(data_width=data_width, mptype=MemoryPortType.R, delay=1)
    ls.register(stg, wr_mem_port, rd_mem_port, stg)

    # 5. Now connect registered components
    # In to In
    ls.connect(in_port, in_id)
    ls.connect(in_port, in_ag)
    ls.connect(in_port, in_sg)
    # Out to Out
    ls.connect(out_port, out_id)
    ls.connect(out_port, out_ag)
    ls.connect(out_port, out_sg)
    # In and Out to MemoryPorts
    ls.connect(in_port, wr_mem_port)
    ls.connect(out_port, rd_mem_port)
    # MemoryPorts to Storage
    ls.connect(wr_mem_port, stg)
    ls.connect(rd_mem_port, stg)

    return ls
