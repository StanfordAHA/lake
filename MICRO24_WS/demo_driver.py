
from lake.utils.spec_enum import Direction
from lake.utils.util import TestPrepper, get_data_sizes, calculate_read_out
import os as os
import argparse
from simple_dual_port import build_simple_dual_port_demo


def get_linear_test(depth=512):

    linear_test = {}

    use_depth = 64
    if depth < 64:
        use_depth = depth

    outer = 32

    linear_test[0] = {
        'type': Direction.IN,
        'name': 'port_w0',
        'config': {
            'dimensionality': 2,
            'extents': [use_depth, outer],
            'address': {
                'strides': [1, 0],
                'offset': 0
            },
            'schedule': {
                'strides': [1, use_depth],
                'offset': 0
            }
        }
    }

    linear_test[1] = {
        'type': Direction.OUT,
        'name': 'port_r0',
        'config': {
            'dimensionality': 2,
            'extents': [use_depth, outer],
            'address': {
                'strides': [1, 0],
                'offset': 0
            },
            'schedule': {
                'strides': [1, use_depth],
                'offset': 1
            }
        }
    }

    return linear_test


def test_linear_read_write(output_dir=None, storage_capacity=1024, data_width=16,
                           tp: TestPrepper = None, reg_file=False, dimensionality=6,
                           verilog=False):

    assert tp is not None

    # Put it at the lake directory by default
    if output_dir is None:
        output_dir = os.path.dirname(os.path.abspath(__file__))
        output_dir = output_dir + "/../../"

    output_dir_verilog = os.path.join(output_dir, 'inputs')

    # Build the spec
    simple_dual_port_spec = build_simple_dual_port_demo(storage_capacity=storage_capacity, data_width=data_width,
                                                        reg_file=reg_file, dims=dimensionality)
    simple_dual_port_spec.visualize_graph()
    simple_dual_port_spec.generate_hardware()
    simple_dual_port_spec.extract_compiler_information()

    # output this to the inputs thing
    if verilog is True:
        print(f"Generating verilog at {output_dir_verilog}")
        simple_dual_port_spec.get_verilog(output_dir=output_dir_verilog)

    mem_depth = storage_capacity // (data_width // 8)

    # Define the test
    lt = get_linear_test(depth=mem_depth)

    max_time = 0
    read_outs = calculate_read_out(lt)
    # Now we have the output sequences
    # Need to write them out
    for pnum, sequences in read_outs.items():
        port_name = lt[pnum]['name']
        times = sequences['time']
        datas = sequences['data']
        if times[-1] > max_time:
            max_time = times[-1]
        # Need to add a cycle delay if using SRAM or reg file
        times = [time + 1 for time in times]
        gold_output_path_data = os.path.join(output_dir, "gold", f"{port_name}_data.txt")
        gold_output_path_time = os.path.join(output_dir, "gold", f"{port_name}_time.txt")
        with open(gold_output_path_data, 'w') as file:
            for data_ in datas:
                file.write(f"{data_}\n")
        with open(gold_output_path_time, 'w') as file:
            for time_ in times:
                file.write(f"{time_}\n")

    # Now generate the bitstream to a file (will be loaded in test harness later)
    bs = simple_dual_port_spec.gen_bitstream(lt)

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
    tp.add_pargs(data_sizes)
    # tp.add_pargs(('max_time', max_time + int((max_time / 10))))
    tp.add_pargs(('max_time', max_time + 15))
    tp.add_pargs(('static', 1))


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Simple Dual Port')
    parser.add_argument("--storage_capacity", type=int, default=1024)
    parser.add_argument("--data_width", type=int, default=16)
    parser.add_argument("--reg_file", action="store_true")
    parser.add_argument("--clock_count_width", type=int, default=32)
    parser.add_argument("--dimensionality", type=int, default=6)
    parser.add_argument("--tech", type=str, default="GF")
    parser.add_argument("--verilog", action="store_true")
    parser.add_argument("--outdir", type=str, default=None)
    args = parser.parse_args()

    print("Preparing hardware test")

    # argparser

    tp = TestPrepper(base_dir=args.outdir)
    hw_test_dir = tp.prepare_hw_test()

    print(f"Put hw test at {hw_test_dir}")

    test_linear_read_write(output_dir=hw_test_dir, storage_capacity=args.storage_capacity, data_width=args.data_width,
                           tp=tp, reg_file=args.reg_file, dimensionality=args.dimensionality, verilog=args.verilog)
