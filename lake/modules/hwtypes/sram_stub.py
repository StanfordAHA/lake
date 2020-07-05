from peak import Peak, family_closure, Const, name_outputs, family
from math import log2
from utils import *

import magma as m
import hwtypes
import fault
import tempfile
import random as rand


def sram_stub(
        mem_depth=4,
        word_width=16,
        fetch_width=4,
        family=family.PyFamily()):

    data_width = word_width * fetch_width
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

                # print("wen: ", wen, " cen: ", cen, " data_in ", data_in, " addr: ", addr)
                # print("mem: ", self.mem)
                if (cen == Bit(1)) & (wen == Bit(1)):
                    # print("setting slice")
                    self.mem = set_slice(self.mem, addr, data_width, data_in)
                    # print("mem after set slice ", self.mem)

                if (cen == Bit(1)) & (wen == Bit(0)):
                    # print("getting slice")
                    # print("get slice mem ", self.mem)
                    data_out = get_slice(self.mem, addr * data_width, data_width)
                else:
                    data_out = WideData(0)

                # print("mem: ", self.mem)
                # print("data out: ", data_out)
                return data_out

        return SRAMStub

    return modules_fc

if __name__ == "__main__":
    mem_depth = 4
    data_width = 16
    fetch_width = 4

    sram_py = sram_stub(mem_depth, data_width, fetch_width, family.PyFamily())
    sram_py_inst = sram_py(family=family.PyFamily())()

    sram_magma = sram_stub(mem_depth, data_width, fetch_width, family=family.MagmaFamily())
    sram_magma_defn = sram_magma(family=family.MagmaFamily())
    tester = fault.Tester(sram_magma_defn, sram_magma_defn.CLK)

    x = 0
    for i in range(15):
        wen = 1 - (i % 2) #rand.randint(0, 1)
        cen = 1 # rand.randint(0, 1)
        addr = x #rand.randint(0, mem_depth - 1)
        data = x #rand.randint(0, 2**(data_width * fetch_width) - 1)
    
        py_data_out = sram_py_inst(hwtypes.Bit(wen), hwtypes.Bit(cen), hwtypes.BitVector[log2(mem_depth)](addr), hwtypes.BitVector[data_width * fetch_width](data))

        if i % 2:
            x = x + 1
        tester.circuit.wen = wen
        tester.circuit.cen = cen
        tester.circuit.addr = addr
        tester.circuit.data_in = data
        tester.eval()
        if cen and (not wen):
            tester.circuit.O.expect(py_data_out)
        # print(py_data_out)
        tester.step(2)

        with tempfile.TemporaryDirectory() as tempdir:
            tempdir="output_m"
            tester.compile_and_run(target="verilator",
                                   directory=tempdir,
                                   flags=["-Wno-fatal", "--trace"])
