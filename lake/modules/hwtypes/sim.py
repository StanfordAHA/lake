from peak import Peak, family_closure, Const, name_outputs, family
from types import SimpleNamespace
import magma as m
from math import log2

import fault

def sram_stub(mem_depth=4,
              data_width=64,
              family=family.PyFamily()):

    num_bits = mem_depth * data_width
    addr_width = log2(mem_depth)

    @family_closure
    def modules_fc(family):
        BitVector = family.BitVector
        Bit = family.Bit
        WideData = family.BitVector[data_width]

        @family.assemble(locals(), globals())
        class SRAMStub():
            def __init__(self):

                self.mem: BitVector[num_bits](0)

            @name_outputs(data_out=WideData)
            def __call__(self, 
                wen: Bit = Bit(0), \
                cen: Bit = Bit(0), \
                addr: BitVector[addr_width] = BitVector[addr_width](0), \
                data_in: WideData = WideData(0)
                ) -> (WideData):

                if cen == Bit(1) & wen == Bit(1):
                    self.mem[addr * data_width, (addr + 1) * data_width] = data_in

                if cen == Bit(1) & wen == Bit(0):
                    data_out = self.mem[addr * data_width, (addr + 1) * data_width]
                else:
                    data_out = WideData(0)

                return data_out

    return modules_fc

if __name__ == "__main__":
    sram_stub()
