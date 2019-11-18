from lake.modules.transpose_buffer_kratos import *
import magma as m
from magma import *
import fault
import tempfile
from kratos import Simulator

word_width = 1 
mem_word_width = 4
range_ = 1
stride = 1
stencil_height = 5
dut = TransposeBuffer(word_width, mem_word_width, range_, stride, stencil_height)
verilog(dut, filename="transposebuffer.sv")
'''kratos_tb = kratos.create_stub(dut, flatten_array=True)
print(kratos_tb)
magma_tb = m.DefineFromVerilog(kratos_tb, type_map={"clk": m.In(m.Clock), "rst_n": m.In(m.AsyncReset)})[0]
print(magma_tb)
tester = fault.Tester(magma_tb, magma_tb.clk)
'''

sim = Simulator(dut)
sim.reset()
data = [0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1]
sim.set(dut.valid_input, [1,1,1,1])
for i in range(16):
    sim.set(dut.rst_n, 1)
    sim.set(dut.mem_data, data[i*4:(i+1)*4])
    sim.cycle()
    print("max dim:", sim.get(dut.max_dim), "pause input:", sim.get(dut.pause_input), "pause output:", sim.get(dut.pause_output),  "mem_data:", sim.get(dut.mem_data), "col_index:", sim.get(dut.col_index), "row_index:", sim.get(dut.row_index), "switch:", sim.get(dut.switch_buf),  "rst:", sim.get(dut.rst_n), "valid_data:", sim.get(dut.valid_data), "col_pixels:", sim.get(dut.col_pixels))
    print("tb:", sim.get(dut.tb), "output valid:", sim.get(dut.output_valid))
    print()


'''
tester.circuit.clk = 0
tester.circuit.rst = 0
tester.step(2)
tester.circuit.rst_n = 1
b = [1,1,1,0]
c = [1,1,1,1]
for i in range(2*mem_word_width + 1):
    tester.circuit.mem_data = b
    tester.valid_input = c
    tester.eval()
    tester.step(2)

with tempfile.TemporaryDirectory() as tempdir:
    tempdir="tb_dump"
    tester.compile_and_run(target="verilator",
                           skip_compile=True,
                           directory=tempdir,
                           flags=["-Wno-fatal", "--trace"])
'''
