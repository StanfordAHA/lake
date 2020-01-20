from lake.modules.transpose_buffer import *
import magma as m
from magma import *
import fault
import tempfile
from kratos import *
# from kratos import Simulator

word_width = 1
fetch_width = 4
stencil_height = 3
max_range_value = 5
max_img_height = 5
num_tb = 1
dut = TransposeBuffer(word_width, fetch_width, num_tb, stencil_height, max_range_value, max_img_height)
magma_dut = kratos.util.to_magma(dut, flatten_array=True)
verilog(dut, filename="transposebuffer.sv")
tester = fault.Tester(magma_dut, magma_dut.clk)
tester.circuit.clk = 0
tester.circuit.rst_n = 1
tester.step(2)
tester.circuit.rst_n = 0
tester.step(2)
tester.circuit.rst_n = 1
for i in range(21):
    for j in range(fetch_width):
        setattr(tester.circuit, f"input_data_{j}", j % 2)
    tester.circuit.range_outer = 5
    tester.circuit.range_inner = 3
    tester.circuit.stride = 2
    tester.circuit.img_height = 4
    for j in range(max_range_value):
        setattr(tester.circuit, f"indices_{j}", j)
    tester.eval()
    tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir="/nobackupkiwi/skavya/lake/tests/temp"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal", "--trace"],
                               magma_output="verilog")


'''
word_width = 1 
mem_word_width = 4 
stencil_height = 5
stencil_width = 5
num_output = 5
dut = TransposeBuffer(word_width, mem_word_width, stencil_height, stencil_width, num_output)
verilog(dut, filename="vtransposebuffer.sv")

sim = Simulator(dut)
sim.reset()
data = [0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1]
sim.set(dut.valid_input, [1,1,1,1])
sim.set(dut.out_indices, [0,0,2,1,1])
sim.set(dut.out_indices_valid, [1,1,1,1,1])
for i in range(16):
    sim.set(dut.mem_data, data[i*4:(i+1)*4])
    sim.set(dut.rst_n, 1)

    if i == 0:
        print("max dim:", sim.get(dut.max_dim), "pause input:", sim.get(dut.pause_input), "pause output:", sim.get(dut.pause_output),  "mem_data:", sim.get(dut.mem_data), "col_index:", sim.get(dut.col_index), "row_index:", sim.get(dut.row_index), "switch:", sim.get(dut.switch_buf),  "rst:", sim.get(dut.rst_n), "valid_data:", sim.get(dut.valid_data), "col_pixels:", sim.get(dut.col_pixels))
        print("tb:", sim.get(dut.tb), "output valid:", sim.get(dut.output_valid), "stencil valid:", sim.get(dut.stencil_valid), "valid_cols_count:", sim.get(dut.valid_cols_count))
        print()

    sim.cycle()
    print("max dim:", sim.get(dut.max_dim), "pause input:", sim.get(dut.pause_input), "pause output:", sim.get(dut.pause_output),  "mem_data:", sim.get(dut.mem_data), "col_index:", sim.get(dut.col_index), "row_index:", sim.get(dut.row_index), "switch:", sim.get(dut.switch_buf),  "rst:", sim.get(dut.rst_n), "valid_data:", sim.get(dut.valid_data), "col_pixels:", sim.get(dut.col_pixels))
    print("tb:", sim.get(dut.tb), "output valid:", sim.get(dut.output_valid), "stencil valid:", sim.get(dut.stencil_valid), "valid_cols_count:", sim.get(dut.valid_cols_count))
    print("valid_out_indices:", sim.get(dut.valid_out_indices))
    print("num out", sim.get(dut.num_out))
    print("index", sim.get(dut.index))
    print()
'''
