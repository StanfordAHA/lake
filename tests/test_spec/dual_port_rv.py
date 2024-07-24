from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType, LFComparisonOperator
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ReadyValidScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.utils.util import prepare_hw_test, get_data_sizes, TestPrepper
from lake.top.tech_maps import GF_Tech_Map
import os as os
import argparse


def build_simple_dual_port(storage_capacity: int = 1024, data_width=16,
                           dims: int = 6, clock_count_width=16, physical=False,
                           recurrence=True) -> Spec:

    ls = Spec()

    in_port = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC, direction=Direction.IN)
    out_port = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC, direction=Direction.OUT)

    ls.register(in_port, out_port)

    in_id = IterationDomain(dimensionality=dims, extent_width=clock_count_width)
    in_ag = AddressGenerator(dimensionality=dims, recurrence=recurrence)
    in_sg = ReadyValidScheduleGenerator(dimensionality=dims, recurrence=recurrence)

    out_id = IterationDomain(dimensionality=dims, extent_width=clock_count_width)
    out_ag = AddressGenerator(dimensionality=dims, recurrence=recurrence)
    out_sg = ReadyValidScheduleGenerator(dimensionality=dims, recurrence=recurrence)

    ls.register(in_id, in_ag, in_sg)
    ls.register(out_id, out_ag, out_sg)

    # 1024 Bytes
    data_bytes = data_width // 8

    tech_map = None
    if physical:
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes, width=data_width, dual_port=True)

    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map)
    wr_mem_port = MemoryPort(data_width=16, mptype=MemoryPortType.W, delay=1)
    rd_mem_port = MemoryPort(data_width=16, mptype=MemoryPortType.R, delay=1)
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


def get_linear_test():

    linear_test = {}

    linear_test[0] = {
        'type': Direction.IN,
        'name': 'write_port_0',
        'config': {
            'dimensionality': 1,
            'extents': [64],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [1],
                'offset': 0
            }
        }
    }

    linear_test[1] = {
        'type': Direction.OUT,
        'name': 'read_port_0',
        'config': {
            'dimensionality': 1,
            'extents': [64],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [1],
                'offset': 16
            }
        }
    }

    pw = 0
    pr = 1

    pr_raw_idx = 0
    pw_raw_idx = 0
    raw_comp = LFComparisonOperator.LT.value
    raw_scalar = 0
    raw_constraint = (pr, pr_raw_idx, pw, pw_raw_idx, raw_comp, raw_scalar)

    pw_war_idx = 0
    pr_war_idx = 0
    war_comp = LFComparisonOperator.LT.value
    war_scalar = 16
    war_constraint = (pw, pw_war_idx, pr, pr_war_idx, war_comp, war_scalar)

    # Just have read follow write
    linear_test['constraints'] = [raw_constraint, war_constraint]

    return linear_test


def test_linear_read_write(output_dir=None, storage_capacity=1024, data_width=16, clock_count_width=64,
                           physical=False, tp: TestPrepper = None):

    # Put it at the lake directory by default
    if output_dir is None:
        output_dir = os.path.dirname(os.path.abspath(__file__))
        output_dir = output_dir + "/../../"

    output_dir_verilog = os.path.join(output_dir, 'inputs')

    print(f"putting verilog at {output_dir_verilog}")
    # Build the spec
    simple_dual_port_spec = build_simple_dual_port(storage_capacity=storage_capacity, data_width=data_width, physical=physical)
    simple_dual_port_spec.visualize_graph()
    simple_dual_port_spec.generate_hardware()
    simple_dual_port_spec.extract_compiler_information()

    # output this to the inputs thing
    simple_dual_port_spec.get_verilog(output_dir=output_dir_verilog)

    # Define the test
    lt = get_linear_test()

    # Now generate the bitstream to a file (will be loaded in test harness later)
    bs = simple_dual_port_spec.gen_bitstream(lt)

    print('final bs')
    print(bs)

    bin_rep = bin(bs)
    print(bin_rep)
    print(f"'b{bin_rep[2:]}")

    # Convert the number to a hexadecimal string
    hex_string = hex(bs)[2:]  # Remove the '0x' prefix

    bs_output_path = os.path.join(output_dir, "inputs", "bitstream.bs")

    print(f"bitstream path {bs_output_path}")

    # Write the hexadecimal string to the input folders
    with open(bs_output_path, 'w') as file:
        file.write(hex_string)

    # Write out the preprocessor args to inputs
    cfgsz_output_path = os.path.join(output_dir, "inputs", "comp_args.txt")
    config_size = simple_dual_port_spec.get_total_config_size()
    config_define_str = f"+define+CONFIG_MEMORY_SIZE={config_size}\n"
    # Write out num ports for preprocessor arg
    num_ports = simple_dual_port_spec.get_num_ports()
    numports_define_str = f"+define+NUMBER_PORTS={num_ports}\n"

    with open(cfgsz_output_path, 'w') as file:
        file.write(config_define_str)
        file.write(numports_define_str)

    data_sizes = get_data_sizes(lt, num_ports=2)
    # Trying to use the test preparation tool
    assert tp is not None
    tp.add_pargs(data_sizes)
    tp.add_pargs(('static', 0))


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Simple Dual Port RV')
    parser.add_argument("--storage_capacity", type=int, default=1024)
    parser.add_argument("--data_width", type=int, default=16)
    parser.add_argument("--clock_count_width", type=int, default=64)
    parser.add_argument("--tech", type=str, default="GF")
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--outdir", type=str, default=None)
    args = parser.parse_args()

    print("Preparing hardware test")

    # argparser

    tp = TestPrepper(base_dir=args.outdir)
    hw_test_dir = tp.prepare_hw_test()

    # hw_test_dir = prepare_hw_test(base_dir=args.outdir)
    print(f"Put hw test at {hw_test_dir}")

    test_linear_read_write(output_dir=hw_test_dir, storage_capacity=args.storage_capacity, data_width=args.data_width,
                           clock_count_width=args.clock_count_width, physical=args.physical,
                           tp=tp)
