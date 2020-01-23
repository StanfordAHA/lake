word_width = 1
mem_word_width = 4
stencil_height = 5
stencil_width = 5
num_output = 5
dut = TransposeBuffer(word_width, mem_word_width, stencil_height, stencil_width, num_output)
verilog(dut, filename="vtransposebuffer.sv")

sim = Simulator(dut)
sim.reset()
data = [0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1
,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1]
sim.set(dut.valid_input, [1,1,1,1])
sim.set(dut.out_indices, [0,0,2,1,1])
sim.set(dut.out_indices_valid, [1,1,1,1,1])
for i in range(16):
    sim.set(dut.mem_data, data[i*4:(i+1)*4])
    sim.set(dut.rst_n, 1)

    if i == 0:
        print("max dim:", sim.get(dut.max_dim), "pause input:", sim.get(dut.pause_input), "pause output:",
        sim.get(dut.pause_output),  "mem_data:", sim.get(dut.mem_data), "col_index:",
        sim.get(dut.col_index), "row_index:", sim.get(dut.row_index), "switch:", sim.get(dut.switch_buf),
        "rst:", sim.get(dut.rst_n), "valid_data:", sim.get(dut.valid_data),
        "col_pixels:", sim.get(dut.col_pixels))
        print("tb:", sim.get(dut.tb), "output valid:", sim.get(dut.output_valid), "stencil valid:",
        sim.get(dut.stencil_valid), "valid_cols_count:", sim.get(dut.valid_cols_count))
        print()

    sim.cycle()
    print("max dim:", sim.get(dut.max_dim), "pause input:", sim.get(dut.pause_input), "pause output:",
    sim.get(dut.pause_output),  "mem_data:", sim.get(dut.mem_data), "col_index:", sim.get(dut.col_index),
    "row_index:", sim.get(dut.row_index), "switch:", sim.get(dut.switch_buf),  "rst:", sim.get(dut.rst_n),
    "valid_data:", sim.get(dut.valid_data), "col_pixels:", sim.get(dut.col_pixels))
    print("tb:", sim.get(dut.tb), "output valid:", sim.get(dut.output_valid), "stencil valid:",
    sim.get(dut.stencil_valid), "valid_cols_count:", sim.get(dut.valid_cols_count))
    print("valid_out_indices:", sim.get(dut.valid_out_indices))
    print("num out", sim.get(dut.num_out))
    print("index", sim.get(dut.index))
    print()

