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
