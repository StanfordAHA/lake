from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.storage import SingleBankStorage, Storage
from lake.spec.memory_port import MemoryPort


def build_single_port_wide_fetch_multiple_ports(dims: int = 6, width=4) -> Spec:

    ls = Spec()

    dw = 16

    in_port = Port(ext_data_width=dw, int_data_width=dw * width, runtime=Runtime.STATIC, direction=Direction.IN)
    out_port = Port(ext_data_width=dw, int_data_width=dw * width, runtime=Runtime.STATIC, direction=Direction.OUT)
    in_port2 = Port(ext_data_width=dw, int_data_width=dw * width, runtime=Runtime.STATIC, direction=Direction.IN)
    out_port2 = Port(ext_data_width=dw, int_data_width=dw * width, runtime=Runtime.STATIC, direction=Direction.OUT)

    ls.register(in_port, out_port, in_port2, out_port2)

    in_id = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag = AddressGenerator(dimensionality=dims)
    in_sg = ScheduleGenerator(dimensionality=dims)

    in_id2 = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag2 = AddressGenerator(dimensionality=dims)
    in_sg2 = ScheduleGenerator(dimensionality=dims)

    out_id = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag = AddressGenerator(dimensionality=dims)
    out_sg = ScheduleGenerator(dimensionality=dims)

    out_id2 = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag2 = AddressGenerator(dimensionality=dims)
    out_sg2 = ScheduleGenerator(dimensionality=dims)

    ls.register(in_id, in_ag, in_sg, in_id2, in_ag2, in_sg2)
    ls.register(out_id, out_ag, out_sg, out_id2, out_ag2, out_sg2)

    # 1024 Bytes
    stg = SingleBankStorage(capacity=1024)
    wr_mem_port = MemoryPort(data_width=dw * width, mptype=MemoryPortType.W, delay=1)
    rd_mem_port = MemoryPort(data_width=dw * width, mptype=MemoryPortType.R, delay=1)
    ls.register(stg, wr_mem_port, rd_mem_port, stg)

    # All cores are registered at this point
    # Now connect them

    # In to in
    ls.connect(in_port, in_id)
    ls.connect(in_port, in_ag)
    ls.connect(in_port, in_sg)

    # In to in
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

    # In and Out to memory ports
    ls.connect(in_port, wr_mem_port)
    ls.connect(in_port2, wr_mem_port)
    ls.connect(out_port, rd_mem_port)
    ls.connect(out_port2, rd_mem_port)

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


def test_linear_read_write_sp_wf_mp():

    # Build the spec
    lakespec = build_single_port_wide_fetch_multiple_ports()
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

    # simple_dual_port_spec = build_simple_dual_port()
    # simple_dual_port_spec.visualize_graph()
    # simple_dual_port_spec.generate_hardware()
    # simple_dual_port_spec.extract_compiler_information()
    # simple_dual_port_spec.get_verilog()

    test_linear_read_write_sp_wf_mp()
