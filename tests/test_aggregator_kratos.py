from lake.modules.aggregator_kratos import *
import magma as m
from magma import *
import fault
import tempfile

# get verilog file that needs to be copied to agg_dump directory before running verilator
mem_word_width = 16
word_width = 64
dut = Aggregator(word_width, mem_word_width)
verilog(dut, filename="aggregator.v")
kratos_agg = kratos.create_stub(dut)

# testing magma circuit with fault
magma_agg = m.DefineFromVerilog(kratos_agg, type_map={"clk": m.In(m.Clock), "rst": m.In(m.AsyncReset)})[0]


tester = fault.Tester(magma_agg, magma_agg.clk)
tester.circuit.clk = 0
# initial reset
tester.circuit.rst = 0
tester.step(2)
tester.circuit.rst = 1

# input test data
for i in range(2*mem_word_width + 1):
    if (i < 2**word_width):
        input_data = i
    else:
        input_data = 2**word_width - 1
    tester.circuit.in_pixels = input_data
    tester.eval()
    tester.step(2)

with tempfile.TemporaryDirectory() as tempdir:
    tempdir="agg_dump"
    tester.compile_and_run(target="verilator",
                           skip_compile=True,
                           directory=tempdir,
                           flags=["-Wno-fatal", "--trace"])
