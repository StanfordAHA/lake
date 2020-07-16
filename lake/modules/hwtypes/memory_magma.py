import magma as m
from hwtypes import Bit, BitVector
from math import log2


def create_circuit(mem_depth=4,
                   word_width=16):

    num_bits = mem_depth * word_width
    addr_width = log2(mem_depth)

    @m.circuit.sequential(async_reset=True)
    class DelayBy2:
        def __init__(self):
            self.mem: m.Bits[num_bits] = m.bits(0, num_bits)

        def __call__(self, wen: m.Bits[1],
                     cen: m.Bits[1],
                     addr: m.Bits[addr_width],
                     data_in: m.Bits[word_width]
                     ) -> (m.Bits[word_width]):

            O = self.y
            self.y = self.x
            self.x = I
            return O

    return DelayBy2


create_circuit()
