import aggregator
from aggregator import *

# Input word_width and memory_width to be tested here
word_width = 1
memory_width = 1

print("word_width: ", word_width)
print("memory_width: ", memory_width)
_Aggregator = Aggregator(word_width, memory_width)
tester = fault.Tester(_Aggregator, _Aggregator.CLK)
tester.circuit.CLK = 0
for i in range(2*memory_width + 1):
    tester.circuit.INPUT_PIXELS = i
    tester.step(2)
tester.eval()
tester.compile_and_run("verilator", flags=["-Wno-fatal", "--trace"], directory="agg")
