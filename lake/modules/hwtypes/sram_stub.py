from peak import Peak, family_closure, Const, name_outputs, family
from math import log2
from utils import *

import magma as m
import hwtypes
import fault

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
                    cen: Bit = Bit(0), \
                    wen: Bit = Bit(0), \
                    addr: BitVector[addr_width] = BitVector[addr_width](0), \
                    data_in: WideData = WideData(0)
                    ) -> (WideData):

                print("wen: ", wen, " cen: ", cen, " data_in ", data_in, " addr: ", addr)
                print("mem: ", self.mem)
                if (cen == Bit(1)) & (wen == Bit(1)):
                    print("setting slice")
                    self.mem = set_slice(self.mem, addr, data_width, data_in)
                    # print("mem after set slice ", self.mem)

                if (cen == Bit(1)) & (wen == Bit(0)):
                    print("getting slice")
                    print("get slice mem ", self.mem)
                    # data_out = self.mem[addr * data_width : (addr + 1) * data_width]
                    data_out = get_slice(self.mem, addr * data_width, data_width)
                else:
                    data_out = WideData(0)

                print("mem: ", self.mem)
                print("data out: ", data_out)
                return data_out

        return SRAMStub

    return modules_fc

if __name__ == "__main__":
    pyt = False
    if pyt:
        mem_depth = 4
        data_width = 64
        sram_py = sram_stub(mem_depth, data_width, family.PyFamily())
        sram_py_inst = sram_py(family=family.PyFamily())()
        a = sram_py_inst(hwtypes.Bit(1), hwtypes.Bit(1), hwtypes.BitVector[log2(mem_depth)](0), hwtypes.BitVector[data_width](1))
        print(a)
        b = sram_py_inst(hwtypes.Bit(1), hwtypes.Bit(0), hwtypes.BitVector[log2(mem_depth)](0), hwtypes.BitVector[data_width](1))
        print(b)
        c = sram_py_inst(hwtypes.Bit(1), hwtypes.Bit(1), hwtypes.BitVector[log2(mem_depth)](1), hwtypes.BitVector[data_width](2))
        print(c)
        d = sram_py_inst(hwtypes.Bit(1), hwtypes.Bit(0), hwtypes.BitVector[log2(mem_depth)](1), hwtypes.BitVector[data_width](2))
        print(d)
    else:
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
   
        with tempfile.TemporaryDirectory() as tempdir:
            tempdir="output"
            tester.compile_and_run(target="verilator", 
                                   directory=tempdir,
                                   magma_output="verilog",
                                   flags=["-Wno-fatal"])

