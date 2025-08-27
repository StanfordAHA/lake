from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.utils.util import TestPrepper, get_data_sizes, calculate_read_out_vec
from lake.top.tech_maps import GF_Tech_Map
import argparse
import os


def build_dual_port_wide_fetch(storage_capacity=1024, data_width=16, dims: int = 6,
                               vec_width=4, physical=False, reg_file=False,
                               vec_capacity=2) -> Spec:

    read_delay = 0 if reg_file else 1

    ls = Spec()

    in_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                   vec_capacity=vec_capacity, runtime=Runtime.STATIC, direction=Direction.IN)
    out_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                    vec_capacity=vec_capacity, runtime=Runtime.STATIC, direction=Direction.OUT)

    ls.register(in_port, out_port)

    in_id = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag = AddressGenerator(dimensionality=dims)
    in_sg = ScheduleGenerator(dimensionality=dims)

    out_id = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag = AddressGenerator(dimensionality=dims)
    out_sg = ScheduleGenerator(dimensionality=dims)

    ls.register(in_id, in_ag, in_sg)
    ls.register(out_id, out_ag, out_sg)

    data_bytes = (data_width * vec_width) // 8
    tech_map = None
    if physical:
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes, width=data_width * vec_width,
                               dual_port=True, reg_file=reg_file)

    # 1024 Bytes
    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map)
    wr_mem_port = MemoryPort(data_width=data_width * vec_width, mptype=MemoryPortType.W, delay=1)
    rd_mem_port = MemoryPort(data_width=data_width * vec_width, mptype=MemoryPortType.R, delay=read_delay)
    ls.register(stg, wr_mem_port, rd_mem_port)

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
        'name': 'port_w0',
        'config': {
            'dimensionality': 1,
            'extents': [64],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 4
            }
        },
        'vec_in_config': {
            'dimensionality': 2,
            'extents': [4, 64],
            'address': {
                'strides': [1, 4],
                'offset': 0
            },
            'schedule': {
                'strides': [1, 4],
                'offset': 0
            }
        },
        'vec_out_config': {
            'dimensionality': 1,
            'extents': [64],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [4],
                'offset': 4
            }
        }
    }

    linear_test[1] = {
        'type': Direction.OUT,
        'name': 'port_r0',
        'config': {
            'dimensionality': 1,
            'extents': [64],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [8],
                'offset': 16
            }
        },
        'vec_in_config': {
            'dimensionality': 1,
            'extents': [64],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [8],
                'offset': 17
            }
        },
        'vec_out_config': {
            'dimensionality': 2,
            'extents': [4, 16],
            'address': {
                'strides': [0, 1],
                'offset': 0
            },
            'schedule': {
                'strides': [1, 4],
                'offset': 18
            }
        }
    }

    return linear_test


def test_linear_read_write_dp_wf(output_dir=None, storage_capacity=1024, data_width=16, physical=False, vec_width=4,
                                 tp: TestPrepper = None, reg_file=False, dimensionality=6):

    assert tp is not None

    # Put it at the lake directory by default
    if output_dir is None:
        output_dir = os.path.dirname(os.path.abspath(__file__))
        output_dir = output_dir + "/../../"

    output_dir_verilog = os.path.join(output_dir, 'inputs')

    print(f"putting verilog at {output_dir_verilog}")
    # Build the spec
    simple_single_port_spec = build_dual_port_wide_fetch(storage_capacity=storage_capacity, data_width=data_width,
                                                         physical=physical, vec_width=vec_width, reg_file=reg_file,
                                                         dims=dimensionality)
    simple_single_port_spec.visualize_graph()
    simple_single_port_spec.generate_hardware()
    simple_single_port_spec.extract_compiler_information()

    # output this to simple_single_port_specthe inputs thing
    simple_single_port_spec.get_verilog(output_dir=output_dir_verilog)

    # Define the test
    lt = get_linear_test()

    max_time = 0
    read_outs = calculate_read_out_vec(lt, vec=vec_width)
    # Now we have the output sequences
    # Need to write them out
    for pnum, sequences in read_outs.items():
        port_name = lt[pnum]['name']
        times = sequences['time']
        datas = sequences['data']
        if times[-1] > max_time:
            max_time = times[-1]
        # Need to add a cycle delay if using SRAM
        # if reg_file is False:
        #     times = [time + 1 for time in times]
        gold_output_path_data = os.path.join(output_dir, "gold", f"{port_name}_data.txt")
        gold_output_path_time = os.path.join(output_dir, "gold", f"{port_name}_time.txt")
        with open(gold_output_path_data, 'w') as file:
            for data_ in datas:
                file.write(f"{data_}\n")
        with open(gold_output_path_time, 'w') as file:
            for time_ in times:
                file.write(f"{time_}\n")

    # Now generate the bitstream to a file (will be loaded in test harness later)
    bs = simple_single_port_spec.gen_bitstream(lt)

    # Convert the number to a hexadecimal string
    hex_string = hex(bs)[2:]  # Remove the '0x' prefix

    bs_output_path = os.path.join(output_dir, "inputs", "bitstream.bs")

    print(f"bitstream path {bs_output_path}")

    # Write the hexadecimal string to the input folders
    with open(bs_output_path, 'w') as file:
        file.write(hex_string)

    # Write out the preprocessor args to inputs
    cfgsz_output_path = os.path.join(output_dir, "inputs", "comp_args.txt")
    config_size = simple_single_port_spec.get_total_config_size()
    config_define_str = f"+define+CONFIG_MEMORY_SIZE={config_size}\n"
    # Write out num ports for preprocessor arg
    num_ports = simple_single_port_spec.get_num_ports()
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
    parser.add_argument("--fetch_width", type=int, default=4)
    parser.add_argument("--reg_file", action="store_true")
    parser.add_argument("--dimensionality", type=int, default=6)
    parser.add_argument("--clock_count_width", type=int, default=64)
    parser.add_argument("--tech", type=str, default="GF")
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--outdir", type=str, default=None)
    args = parser.parse_args()

    print("Preparing hardware test")

    # argparser

    tp = TestPrepper(base_dir=args.outdir)
    hw_test_dir = tp.prepare_hw_test()
    print(f"Put hw test at {hw_test_dir}")

    test_linear_read_write_dp_wf(output_dir=hw_test_dir, storage_capacity=args.storage_capacity, data_width=args.data_width,
                                 physical=args.physical, vec_width=args.fetch_width, tp=tp, reg_file=args.reg_file,
                                 dimensionality=args.dimensionality)
