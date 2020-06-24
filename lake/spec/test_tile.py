from tile import *

def test_tile():
    memtile = tile("empty_tile")
    memtile.addInPort(name = "in", word_width = 16)
    memtile.addOutPort(name = "out", word_width = 16)
    memtile.generate_verilog()

def test_sram_tile():
    memtile = tile("sram_tile")
    memtile.addInPort(name = "in", word_width = 16)
    memtile.addOutPort(name = "out", word_width = 16)
    #memtile.addRAM(name = "sram", width = 16, depth = 512, port = 2)
    #memtile.addAccessor(name = "input_accessor", dim = 2)
    memtile.connect("input_accessor", "sram")
    memtile.connect("input_accessor", "in")
    memtile.connect("output_accessor", "sram")
    memtile.connect("output_accessor", "out")
    memtile.generate_compiler_target()

if __name__ == "__main__":
    test_tile()
    test_sram_tile()
