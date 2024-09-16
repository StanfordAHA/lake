from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.utils.util import get_data_sizes, TestPrepper, calculate_read_out_vec
from lake.top.tech_maps import GF_Tech_Map
import argparse
import os


def build_single_port_wide_fetch(storage_capacity=1024, data_width=16, dims: int = 6, vec_width=4,
                                 physical=False, in_ports=1, out_ports=1, vec_capacity=2,
                                 spst="dense") -> Spec:

    ls = Spec()

    # Need to build a number of input and output ports
    all_ins = []
    all_ins_id = []
    all_ins_ag = []
    all_ins_sg = []
    all_outs = []
    all_outs_id = []
    all_outs_ag = []
    all_outs_sg = []
    #  Create input ports and register them
    for inp in range(in_ports):
        in_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width, vec_capacity=vec_capacity, runtime=Runtime.STATIC, direction=Direction.IN)
        ls.register(in_port)
        all_ins.append(in_port)

    # Create outputs ports and register them
    for outp in range(out_ports):
        out_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width, vec_capacity=vec_capacity, runtime=Runtime.STATIC, direction=Direction.OUT)
        ls.register(out_port)
        all_outs.append(out_port)

    # Create input port controllers and register them
    for inp in range(in_ports):
        in_id = IterationDomain(dimensionality=dims, extent_width=16)
        in_ag = AddressGenerator(dimensionality=dims)
        in_sg = ScheduleGenerator(dimensionality=dims)
        ls.register(in_id, in_ag, in_sg)

        all_ins_id.append(in_id)
        all_ins_ag.append(in_ag)
        all_ins_sg.append(in_sg)

    # Same for output
    for outp in range(out_ports):
        out_id = IterationDomain(dimensionality=dims, extent_width=16)
        out_ag = AddressGenerator(dimensionality=dims)
        out_sg = ScheduleGenerator(dimensionality=dims)
        ls.register(out_id, out_ag, out_sg)
        all_outs_id.append(out_id)
        all_outs_ag.append(out_ag)
        all_outs_sg.append(out_sg)


    data_bytes = (data_width * vec_width) // 8
    tech_map = None

    pd_arg = "D"
    if spst == "perf":
        pd_arg = "P"

    if physical:
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes, width=data_width * vec_width, dual_port=False, pd=pd_arg)

    # 1024 Bytes
    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map)
    shared_rw_mem_port = MemoryPort(data_width=data_width * vec_width, mptype=MemoryPortType.RW, delay=1)
    ls.register(stg, shared_rw_mem_port)

    # All cores are registered at this point
    # Now connect them
        # Out to out
    for inp in range(in_ports):
        ls.connect(all_ins[inp], all_ins_id[inp])
        ls.connect(all_ins[inp], all_ins_ag[inp])
        ls.connect(all_ins[inp], all_ins_sg[inp])
        # ls.connect(in_port, in_ag)
        # ls.connect(in_port, in_sg)

    for outp in range(out_ports):
        ls.connect(all_outs[outp], all_outs_id[outp])
        ls.connect(all_outs[outp], all_outs_ag[outp])
        ls.connect(all_outs[outp], all_outs_sg[outp])
        # ls.connect(out_port, out_id)
        # ls.connect(out_port, out_ag)
        # ls.connect(out_port, out_sg)

    # for inp in range(in_ports):
    # In to in
    # ls.connect(in_port, in_id)
    # ls.connect(in_port, in_ag)
    # ls.connect(in_port, in_sg)

    # # Out to out
    # ls.connect(out_port, out_id)
    # ls.connect(out_port, out_ag)
    # ls.connect(out_port, out_sg)

    for inp in range(in_ports):
    # In and Out to shared memory port
        ls.connect(all_ins[inp], shared_rw_mem_port)
    for outp in range(out_ports):
        ls.connect(all_outs[outp], shared_rw_mem_port)
    # ls.connect(in_port, shared_rw_mem_port)
    # ls.connect(out_port, shared_rw_mem_port)

    # Memory Ports to storage
    ls.connect(shared_rw_mem_port, stg)

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
                'offset': 17
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
                'offset': 18
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
                'offset': 19
            }
        }
    }

    return linear_test


def test_linear_read_write_sp_wf(output_dir=None, storage_capacity=1024, data_width=16, physical=False, vec_width=4,
                                 tp: TestPrepper = None, reg_file=False, dimensionality=6, in_ports=1, out_ports=1,
                                 spst="dense"):

    assert tp is not None

    # Put it at the lake directory by default
    if output_dir is None:
        output_dir = os.path.dirname(os.path.abspath(__file__))
        output_dir = output_dir + "/../../"

    output_dir_verilog = os.path.join(output_dir, 'inputs')

    print(f"putting verilog at {output_dir_verilog}")
    # Build the spec
    simple_single_port_spec = build_single_port_wide_fetch(storage_capacity=storage_capacity, data_width=data_width,
                                                           physical=physical, vec_width=vec_width, dims=dimensionality,
                                                           in_ports=in_ports, out_ports=out_ports, spst=spst)
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
    parser.add_argument("--in_ports", type=int, default=1)
    parser.add_argument("--out_ports", type=int, default=1)
    parser.add_argument("--dimensionality", type=int, default=6)
    parser.add_argument("--clock_count_width", type=int, default=64)
    parser.add_argument("--tech", type=str, default="GF")
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--outdir", type=str, default=None)
    parser.add_argument("--spst", type=str, default="dense")
    args = parser.parse_args()

    print("Preparing hardware test")

    # argparser
    fw = args.fetch_width
    if fw < 2:
        print(f"Not parameterized for fetch width of {fw} --- exiting early!")
        exit()

    tp = TestPrepper(base_dir=args.outdir)
    hw_test_dir = tp.prepare_hw_test()
    print(f"Put hw test at {hw_test_dir}")

    test_linear_read_write_sp_wf(output_dir=hw_test_dir, storage_capacity=args.storage_capacity, data_width=args.data_width,
                                 physical=args.physical, vec_width=fw, tp=tp, in_ports=args.in_ports, out_ports=args.out_ports,
                                 dimensionality=args.dimensionality, spst=args.spst)
