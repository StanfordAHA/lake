"""Generate LakeCollateral JSON from a Spec definition.

Usage:
    python -m lake.utils.generate_collateral -o collateral.json [--level mem]

This generates a reference collateral JSON file that can be consumed by
clockwork via the LAKE_COLLATERAL_JSON_<LEVEL> environment variable or
CodegenOptions::load_memory_hierarchy_from_file().
"""
import argparse
import json
import sys
from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.spec.iteration_domain import IterationDomain
from lake.spec.address_generator import AddressGenerator
from lake.spec.schedule_generator import ScheduleGenerator
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType


def build_mem_level_spec():
    """Build the reference 3-storage agg/sram/tb Spec matching LakeCollateral('mem').

    Topology: 2 input ports -> agg (2 banks) -> sram (1 bank) -> tb (2 banks) -> 2 output ports
    """
    ls = Spec()

    in_port_0 = Port(ext_data_width=16, int_data_width=64,
                     vec_capacity=2,
                     runtime=Runtime.STATIC, direction=Direction.IN)
    in_port_1 = Port(ext_data_width=16, int_data_width=64,
                     vec_capacity=2,
                     runtime=Runtime.STATIC, direction=Direction.IN)
    ls.register(in_port_0, in_port_1)

    out_port_0 = Port(ext_data_width=16, int_data_width=64,
                      vec_capacity=2,
                      runtime=Runtime.STATIC, direction=Direction.OUT)
    out_port_1 = Port(ext_data_width=16, int_data_width=64,
                      vec_capacity=2,
                      runtime=Runtime.STATIC, direction=Direction.OUT)
    ls.register(out_port_0, out_port_1)

    for port in [in_port_0, in_port_1]:
        id_ = IterationDomain(dimensionality=6, extent_width=16)
        ag_ = AddressGenerator(dimensionality=6)
        sg_ = ScheduleGenerator(dimensionality=6)
        ls.register(id_, ag_, sg_)
        ls.connect(port, id_)
        ls.connect(port, ag_)
        ls.connect(port, sg_)

    for port in [out_port_0, out_port_1]:
        id_ = IterationDomain(dimensionality=6, extent_width=16)
        ag_ = AddressGenerator(dimensionality=6)
        sg_ = ScheduleGenerator(dimensionality=6)
        ls.register(id_, ag_, sg_)
        ls.connect(port, id_)
        ls.connect(port, ag_)
        ls.connect(port, sg_)

    agg = SingleBankStorage(capacity=8)
    agg_wr_mp_0 = MemoryPort(data_width=16, mptype=MemoryPortType.W, delay=0)
    agg_wr_mp_1 = MemoryPort(data_width=16, mptype=MemoryPortType.W, delay=0)
    ls.register(agg, agg_wr_mp_0, agg_wr_mp_1)
    ls.connect(in_port_0, agg_wr_mp_0)
    ls.connect(in_port_1, agg_wr_mp_1)
    ls.connect(agg_wr_mp_0, agg)
    ls.connect(agg_wr_mp_1, agg)

    sram = SingleBankStorage(capacity=512)
    sram_rw_mp = MemoryPort(data_width=64, mptype=MemoryPortType.RW, delay=0)
    ls.register(sram, sram_rw_mp)
    ls.connect(sram_rw_mp, sram)

    tb = SingleBankStorage(capacity=8)
    tb_rd_mp_0 = MemoryPort(data_width=16, mptype=MemoryPortType.R, delay=0)
    tb_rd_mp_1 = MemoryPort(data_width=16, mptype=MemoryPortType.R, delay=0)
    ls.register(tb, tb_rd_mp_0, tb_rd_mp_1)
    ls.connect(out_port_0, tb_rd_mp_0)
    ls.connect(out_port_1, tb_rd_mp_1)
    ls.connect(tb_rd_mp_0, tb)
    ls.connect(tb_rd_mp_1, tb)

    controller_name_map = {agg: 'agg', sram: 'sram', tb: 'tb'}
    return ls, controller_name_map


def main():
    parser = argparse.ArgumentParser(description='Generate LakeCollateral JSON')
    parser.add_argument('-o', '--output', required=True, help='Output JSON file path')
    parser.add_argument('--level', default='mem', help='Memory hierarchy level name')
    args = parser.parse_args()

    if args.level == 'mem':
        ls, cname_map = build_mem_level_spec()
        ls.save_compiler_information(args.output, controller_name_map=cname_map)
    else:
        print(f"Unknown level: {args.level}. Only 'mem' is currently supported.", file=sys.stderr)
        sys.exit(1)

    print(f"Collateral written to: {args.output}")

    # Print summary
    with open(args.output) as f:
        data = json.load(f)
    print(json.dumps(data, indent=2))


if __name__ == '__main__':
    main()
