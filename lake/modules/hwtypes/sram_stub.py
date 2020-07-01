from peak import Peak, family_closure, Const, name_outputs, family
from math import log2

import magma as m
import hwtypes
import fault


def get_slice(data, addr, width):
    return (data >> addr)[:width]

def set_slice(data, addr_, width, value):
    assert value.size == width
    addr = addr_.zext(data.size - addr_.size)
    top_bits = (data >> (addr + width)) << (addr + width)
    mid_bits = value.zext(data.size - width) << addr
    bot_bits = (data << (addr + width)) >> (addr + width)
    return top_bits | mid_bits | bot_bits

def sram_stub(
        mem_depth=4,
        data_width=64,
        family=family.PyFamily()):

    num_bits = mem_depth * data_width
    addr_width = int(log2(mem_depth))

    @family_closure
    def modules_fc(family):
        BitVector = family.BitVector
        Bit = family.Bit
        WideData = family.BitVector[data_width]

        @family.assemble(locals(), globals())
        class SRAMStub():
            def __init__(self):
                self.mem: BitVector[num_bits] = BitVector[num_bits](0)

            @name_outputs(data_out=WideData)
            def __call__(self,
                    wen: Bit = Bit(0), \
                    cen: Bit = Bit(0), \
                    addr: BitVector[addr_width] = BitVector[addr_width](0), \
                    data_in: WideData = WideData(0)
                    ) -> (WideData):

                if cen == Bit(1) and wen == Bit(1):
                    set_slice(self.mem, addr, data_width, data_in)

                if cen == Bit(1) and wen == Bit(0):
                    # data_out = self.mem[addr * data_width : (addr + 1) * data_width]
                    data_out = get_slice(self.mem, addr * data_width, data_width)
                else:
                    data_out = WideData(0)

                return data_out

        return SRAMStub

    return modules_fc

if __name__ == "__main__":
    mem_depth = 4
    data_width = 64
    sram_py = sram_stub(mem_depth, data_width, family.PyFamily())
    sram_py(family=family.PyFamily())()(hwtypes.Bit(1), hwtypes.Bit(1), hwtypes.BitVector[log2(mem_depth)](2), hwtypes.BitVector[data_width](2))

    sram_magma = sram_stub(family=family.MagmaFamily())
    tester = fault.Tester(sram_magma(family=family.MagmaFamily())())
    data = 0
    for i in range(5):
        tester.circuit.wen = i % 2
        tester.circuit.cen = 1
        tester.circuit.data_in = data
        tester.circuit.addr = data
        data = data + 1
        tester.eval()
        tester.step(2)

    tester.compile_and_run("verilator", flags=["-Wno-fatal"])

