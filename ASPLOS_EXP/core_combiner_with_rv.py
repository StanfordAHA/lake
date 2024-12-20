from lake.top.core_combiner import CoreCombiner
from lake.spec.spec_memory_controller import SpecMemoryController, build_four_port_wide_fetch_rv
from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType, LFComparisonOperator
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ReadyValidScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.top.tech_maps import GF_Tech_Map
import kratos as kts
import argparse
import os


if __name__ == "__main__":

    strg_cap = 16384
    fw = 4
    data_width = 16

    # get the spec
    spec = build_four_port_wide_fetch_rv(storage_capacity=strg_cap, data_width=data_width, vec_width=fw)
    # Instantiate the core
    controllers = [SpecMemoryController(spec=spec)]

    mem_width = fw * data_width
    word_bytes = (data_width * fw) // 8
    mem_depth = strg_cap // word_bytes

    core_combiner = CoreCombiner(controllers=controllers,
                                 name="example_core_combiner",
                                 mem_depth=mem_depth,
                                 mem_width=mem_width,)

    print("Generating verilog!")
    # kts.verilog(core_combiner, filename="core_combiner_mek.sv")
    core_combiner.get_verilog(verilog_name="core_combiner_mek.sv")

    # Run the core
    # core_combiner.run()