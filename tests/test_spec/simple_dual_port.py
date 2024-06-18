from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.storage import SingleBankStorage, Storage
from lake.spec.memory_port import MemoryPort
from lake.utils.util import prepare_hw_test
import os as os


def build_simple_dual_port(storage_capacity: int = 1024, dims: int = 6) -> Spec:

    ls = Spec()

    in_port = Port(ext_data_width=16, runtime=Runtime.STATIC, direction=Direction.IN)
    out_port = Port(ext_data_width=16, runtime=Runtime.STATIC, direction=Direction.OUT)

    ls.register(in_port, out_port)

    in_id = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag = AddressGenerator(dimensionality=dims)
    in_sg = ScheduleGenerator(dimensionality=dims)

    out_id = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag = AddressGenerator(dimensionality=dims)
    out_sg = ScheduleGenerator(dimensionality=dims)

    ls.register(in_id, in_ag, in_sg)
    ls.register(out_id, out_ag, out_sg)

    # 1024 Bytes
    stg = SingleBankStorage(capacity=storage_capacity)
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
    
    return linear_test


def test_linear_read_write(output_dir=None, storage_capacity=1024):

    # Put it at the lake directory by default
    if output_dir is None:
        output_dir = os.path.dirname(os.path.abspath(__file__))
        output_dir = output_dir + "/../../"

    output_dir_verilog = os.path.join(output_dir, 'inputs')

    print(f"putting verilog at {output_dir_verilog}")
    # Build the spec
    simple_dual_port_spec = build_simple_dual_port(storage_capacity=storage_capacity)
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

if __name__ == "__main__":

    print("Preparing hardware test")

    # argparser

    hw_test_dir = prepare_hw_test()
    print(f"Put hw test at {hw_test_dir}")

    # simple_dual_port_spec = build_simple_dual_port()
    # simple_dual_port_spec.visualize_graph()
    # simple_dual_port_spec.generate_hardware()
    # simple_dual_port_spec.extract_compiler_information()
    # simple_dual_port_spec.get_verilog()

    test_linear_read_write(output_dir=hw_test_dir)