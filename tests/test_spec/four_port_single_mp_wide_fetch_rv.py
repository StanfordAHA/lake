from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType, LFComparisonOperator
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ReadyValidScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.utils.util import get_data_sizes, TestPrepper
from lake.top.tech_maps import GF_Tech_Map
import argparse
import os


def build_four_port_wide_fetch_rv(storage_capacity=1024, data_width=16, dims: int = 6, vec_width=4, physical=False) -> Spec:

    ls = Spec()

    in_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                   vec_capacity=8, runtime=Runtime.DYNAMIC, direction=Direction.IN)
    in_port2 = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                    vec_capacity=8, runtime=Runtime.DYNAMIC, direction=Direction.IN)
    out_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                    vec_capacity=8, runtime=Runtime.DYNAMIC, direction=Direction.OUT)
    out_port2 = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                     vec_capacity=8, runtime=Runtime.DYNAMIC, direction=Direction.OUT)

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


def get_two_read_test():

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

    linear_test[2] = {
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

    linear_test[3] = {
        'type': Direction.OUT,
        'name': 'read_port_1',
        'config': {
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
        'vec_in_config': {
            'dimensionality': 1,
            'extents': [64],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {
                'strides': [8],
                'offset': 19
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
                'offset': 20
            }
        }
    }

    return linear_test


def get_linear_test():

    linear_test = {}

    pw_vec_w = 0
    pr_vec_w = 1

    pr_raw_idx_vec_w = 0
    pw_raw_idx_vec_w = 1
    raw_comp_vec_w = LFComparisonOperator.LT.value
    raw_scalar_vec_w = 0
    raw_constraint_vec_w = (pr_vec_w, pr_raw_idx_vec_w,
                            pw_vec_w, pw_raw_idx_vec_w, raw_comp_vec_w, raw_scalar_vec_w)

    pw_war_idx_vec_w = 0
    pr_war_idx_vec_w = 1
    war_comp_vec_w = LFComparisonOperator.LT.value
    war_scalar_vec_w = 2
    war_constraint_vec_w = (pw_vec_w, pw_war_idx_vec_w, pr_vec_w,
                            pr_war_idx_vec_w, war_comp_vec_w, war_scalar_vec_w)

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
        },
        'vec_constraints': [raw_constraint_vec_w, war_constraint_vec_w]
    }

    pw_vec_r = 0
    pr_vec_r = 1

    pr_raw_idx_vec_r = 0
    pw_raw_idx_vec_r = 0
    raw_comp_vec_r = LFComparisonOperator.LT.value
    raw_scalar_vec_r = 0
    raw_constraint_vec_r = (pr_vec_r, pr_raw_idx_vec_r,
                            pw_vec_r, pw_raw_idx_vec_r, raw_comp_vec_r, raw_scalar_vec_r)

    pw_war_idx_vec_r = 0
    pr_war_idx_vec_r = 0
    war_comp_vec_r = LFComparisonOperator.LT.value
    war_scalar_vec_r = 2
    war_constraint_vec_r = (pw_vec_r, pw_war_idx_vec_r, pr_vec_r,
                            pr_war_idx_vec_r, war_comp_vec_r, war_scalar_vec_r)

    linear_test[2] = {
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
        },
        'vec_constraints': [raw_constraint_vec_r, war_constraint_vec_r]
    }

    pw = 0
    pr = 2

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


def test_linear_read_write_qp_wf_rv(output_dir=None, storage_capacity=1024, data_width=16, physical=False, vec_width=4,
                                    tp: TestPrepper = None, test='linear'):

    assert tp is not None

    # Put it at the lake directory by default
    if output_dir is None:
        output_dir = os.path.dirname(os.path.abspath(__file__))
        output_dir = output_dir + "/../../"

    output_dir_verilog = os.path.join(output_dir, 'inputs')

    print(f"putting verilog at {output_dir_verilog}")
    # Build the spec
    simple_four_port_spec = build_four_port_wide_fetch_rv(storage_capacity=storage_capacity, data_width=data_width,
                                                         physical=physical, vec_width=vec_width)
    simple_four_port_spec.visualize_graph()
    simple_four_port_spec.generate_hardware()
    simple_four_port_spec.extract_compiler_information()

    # output this to simple_single_port_specthe inputs thing
    simple_four_port_spec.get_verilog(output_dir=output_dir_verilog)

    # Define the test
    lt = None
    if test == 'linear':
        lt = get_linear_test()
    elif test == 'two_read':
        lt = get_two_read_test()
    else:
        raise NotImplementedError(f"Cannot run test: {test}")

    # Now generate the bitstream to a file (will be loaded in test harness later)
    bs = simple_four_port_spec.gen_bitstream(lt)

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
    config_size = simple_four_port_spec.get_total_config_size()
    config_define_str = f"+define+CONFIG_MEMORY_SIZE={config_size}\n"
    # Write out num ports for preprocessor arg
    num_ports = simple_four_port_spec.get_num_ports()
    numports_define_str = f"+define+NUMBER_PORTS={num_ports}\n"

    with open(cfgsz_output_path, 'w') as file:
        file.write(config_define_str)
        file.write(numports_define_str)

    data_sizes = get_data_sizes(lt, num_ports=4)
    tp.add_pargs(data_sizes)
    tp.add_pargs(('static', 0))


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Simple Dual Port')
    parser.add_argument("--storage_capacity", type=int, default=1024)
    parser.add_argument("--data_width", type=int, default=16)
    parser.add_argument("--vec_width", type=int, default=4)
    parser.add_argument("--clock_count_width", type=int, default=64)
    parser.add_argument("--test", type=str, default="linear")
    parser.add_argument("--tech", type=str, default="GF")
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--outdir", type=str, default=None)
    args = parser.parse_args()

    print("Preparing hardware test")

    # argparser

    tp = TestPrepper(base_dir=args.outdir)
    hw_test_dir = tp.prepare_hw_test()
    print(f"Put hw test at {hw_test_dir}")

    test_linear_read_write_qp_wf_rv(output_dir=hw_test_dir, storage_capacity=args.storage_capacity, data_width=args.data_width,
                                 physical=args.physical, vec_width=args.vec_width, tp=tp, test=args.test)
