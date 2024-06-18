from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ReadyValidScheduleGenerator
from lake.spec.storage import SingleBankStorage, Storage
from lake.spec.memory_port import MemoryPort


def build_simple_dual_port_rv(dims: int = 6, data_width: int = 16) -> Spec:

    ls = Spec()

    in_port = Port(ext_data_width=data_width, runtime=Runtime.STATIC, direction=Direction.IN)
    out_port = Port(ext_data_width=data_width, runtime=Runtime.STATIC, direction=Direction.OUT)

    ls.register(in_port, out_port)

    in_id = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag = AddressGenerator(dimensionality=dims)
    in_sg = ReadyValidScheduleGenerator(dimensionality=dims)

    out_id = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag = AddressGenerator(dimensionality=dims)
    out_sg = ReadyValidScheduleGenerator(dimensionality=dims)

    ls.register(in_id, in_ag, in_sg)
    ls.register(out_id, out_ag, out_sg)

    # 1024 Bytes
    stg = SingleBankStorage(capacity=1024)
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


def test_linear_read_write():

    # Build the spec
    lakespec = build_simple_dual_port_rv()
    lakespec.visualize_graph()
    lakespec.generate_hardware()
    lakespec.extract_compiler_information()
    lakespec.get_verilog()

    # Define the test
    lt = get_linear_test()

    # Now generate the bitstream to a file (will be loaded in test harness later)
    bs = lakespec.gen_bitstream(lt)

    print('final bs')
    print(bs)

    bin_rep = bin(bs)
    print(bin_rep)
    print(f"'b{bin_rep[2:]}")

    # Convert the number to a hexadecimal string
    hex_string = hex(bs)[2:]  # Remove the '0x' prefix

    # Write the hexadecimal string to a file
    with open('number_in_hex.txt', 'w') as file:
        file.write(hex_string)

if __name__ == "__main__":

    print("Hello")

    test_linear_read_write()