from peak import Peak, family_closure, Const, name_outputs, family
from math import log2
from utils import *
from lake.models.sram_model import SRAMModel

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
        BitVector = family.Unsigned
        Bit = family.Bit
        WideData = family.Unsigned[data_width]

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
                if cen & wen:
                    # print("setting slice")
                    print("set slice addr ", addr)
                    result = set_slice(self.mem, addr, data_width, data_in)
                    self.mem = result
                    # print("mem after set slice ", self.mem)

                if cen & ~wen:
                    # print("getting slice")
                    # print("get slice mem ", self.mem)
                    data_out = get_slice(self.mem, addr, data_width)
                else:
                    data_out = WideData(0)
                
                # print("mem: ", self.mem)
                # print("mem: ", self.mem)
                # print("data out: ", data_out)
                return data_out

        return SRAMStub

    return modules_fc

if __name__ == "__main__":
    # functional model                                                                   
    sram_py = sram_stub()                                                                
    py_fam = family.PyFamily()                                                           
    py_sram = sram_py(family=py_fam)()                                                   
    mem_depth = 4                                                                        
    data_width = 64                                                                      
    num_bits = mem_depth * data_width                                                    
    addr_width = int(log2(mem_depth))                                                                                                                                                                     
    for wen in (1, 0):                                                                   
        wen = py_fam.Bit(wen)                                                            
        cen = py_fam.Bit(1)                                                              
        for addr in range(4):                                                            
            addr = py_fam.BitVector[addr_width](addr)                                    
            data = py_fam.BitVector[data_width](rand.randrange(0, 1 << data_width))    
            print(f'wen: {wen}, cen: {cen}, addr: {addr}, data: {data}')                 
            print(py_sram(wen, cen, addr, data)) 

    mem_depth = 4
    data_width = 16
    fetch_width = 4
    addr_width = int(log2(mem_depth))

    py_fam = family.PyFamily()
    sram_py = sram_stub(mem_depth, data_width, fetch_width, py_fam)
    sram_py_inst = sram_py(family=py_fam)()

    sram_magma = sram_stub(mem_depth, data_width, fetch_width, family=family.MagmaFamily())
    sram_magma_defn = sram_magma(family=family.MagmaFamily())
    tester = fault.Tester(sram_magma_defn, sram_magma_defn.CLK)

    model_sram = SRAMModel(data_width, fetch_width, mem_depth, 1)

    x = 0
    for i in range(16):
        wen = 1 - (i % 2) #rand.randint(0, 1)
        cen = 1 # rand.randint(0, 1)
        addr = x #rand.randint(0, mem_depth - 1)
        data = x + 1#rand.randint(0, 2**(data_width * fetch_width) - 1)
    
        py_data_out = sram_py_inst(py_fam.Bit(wen), py_fam.Bit(cen), py_fam.BitVector[log2(mem_depth)](addr), py_fam.BitVector[data_width * fetch_width](data))
        print(f'wen: {wen}, cen: {cen}, addr: {addr}, data: {data}')
        model_data_out = model_sram.interact(wen, cen, addr, [0, 0, 0, data])
   #     if cen and not wen:
        print(f'model {model_data_out} py_dat {py_data_out}')
    #        assert model_data_out[0] == py_data_out
        if i % 2:
            x = x + 1

        '''if i % 2:
            x = x + 1
        tester.circuit.wen = wen
        tester.circuit.cen = cen
        tester.circuit.addr = addr
        tester.circuit.data_in = data
        tester.eval()
        if cen and (not wen):
            tester.circuit.O.expect(py_data_out)
        print(model_data_out)
        print(py_data_out)
        # assert model_data_out[0] == py_data_out
        # print(py_data_out)
        tester.step(2)

        with tempfile.TemporaryDirectory() as tempdir:
            tempdir="output_m"
            tester.compile_and_run(target="verilator",
                                   directory=tempdir,
                                   flags=["-Wno-fatal", "--trace"])'''
