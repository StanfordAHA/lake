import magma as m
import mantle
import fault
import math
import hwtypes

from magma import DeclareFromVerilog
from math import ceil, log
from mantle import Register, Mux, XOr
from hwtypes import BitVector

m.config.set_debug_mode(True)

def DefineTransposeBuffer(word_width, memory_width, range_, stride, stencil_height):
    class _TransposeBuffer(m.Circuit):
        name = 'Transpose_Buffer'
        IO = [  'SRAM_INPUT', m.In(m.Array[memory_width, m.Bits[word_width]]), \
                'VALID_INPUT', m.In(m.Array[memory_width, m.Bit]), \
                'VALID_COL_PIXELS', m.Out(m.Array[stencil_height, m.Bits[word_width]]), \
                'READ_VALID', m.Out(m.Bit), \
                'row_index', m.Out(m.Bits[ceil(log(stencil_height, 2))]), \
                'col_index', m.Out(m.Bits[ceil(log(memory_width, 2))]), \
                'out_index', m.Out(m.Bits[ceil(log(memory_width, 2))]), \
                'out_buf', m.Out(m.Bits[1]), \
                'test_buf', m.Out(m.Bits[2]), \

                'TEST', m.Out(m.Array[2, m.Array[memory_width, m.Array[memory_width, m.Bits[1]]]]), \
                'STENCIL_VALID', m.Out(m.Bit)] \
                + m.ClockInterface(has_async_reset=True, has_reset=True)

        @classmethod
        def definition(tb):
            log_stencil_height = ceil(log(stencil_height, 2)) 
            log_memory_width = ceil(log(memory_width, 2)) 
            test_reg = Register(2, has_reset=True)
            test_reg.I <= m.bits(m.uint(test_reg.O) + 1)
            tb.test_buf <= test_reg.O
            
            output_buffer_index = Register(1, has_reset=True)
            read_row_index = Register(log_stencil_height, has_reset=True)
            read_col_index = Register(log_memory_width, has_reset=True)
            out_col_index = Register(log_memory_width, has_reset=True)

            transpose_buffer = m.join([m.join([m.join([Register(word_width) for _ in range(memory_width)]) for __ in range(stencil_height)]) for ___ in range(2)])
            
            tb.out_buf <= output_buffer_index.O
            tb.row_index <= read_row_index.O
            tb.col_index <= read_col_index.O
            tb.out_index <= out_col_index.O

            m.wire(transpose_buffer.O, transpose_buffer.I)

            tb.TEST <= transpose_buffer.O
            
            # store input SRAM data in transpose buffer

            col_index = m.uint(read_col_index.O, log_memory_width)
            for i in range(memory_width):
                col_index = col_index + 1
                transpose_buffer.I[~output_buffer_index.O[0]][read_row_index.O][col_index] <= mantle.mux([
                    transpose_buffer.O[~output_buffer_index.O[0]][read_row_index.O][col_index],
                    tb.SRAM_INPUT[i]
                    ], tb.VALID_INPUT[i] == m.bit(1))
#                col_index = col_index + 1 #mantle.mux([
#                    col_index + 1,
 #                   col_index + 1
  #              ],  tb.VALID_INPUT[i] == m.bit(1))
            
            read_col_index.I <= m.bits(col_index, log_memory_width)

            read_row_index.I <= mantle.mux([m.bits(m.uint(read_row_index.O) + 1), \
                    m.bits(0, log_stencil_height)], \
                    (read_col_index.O == m.bits(memory_width - 1, log_memory_width)))
#            read_col_index.I <= mantle.mux([col_index, \
 #                   col_index], \
                    #m.bits(0, log_memory_width)], \
  #                  ((m.bits(col_index) == m.bits(memory_width - 1, log_memory_width)))) 

            # output pixels to stencil_height shift registers from transpose buffer
            for i in range(stencil_height):
                tb.VALID_COL_PIXELS[i] <= \
                        transpose_buffer.O[output_buffer_index.O[0]][i][out_col_index.O]

            out_col_index.I <= mantle.mux([m.bits(m.uint(out_col_index.O, log_memory_width) + 1), 
                    m.bits(0, log_memory_width)], \
                    ((out_col_index.O == m.bits(memory_width - 1)))) 

            output_buffer_index.I <= mantle.mux([output_buffer_index.O,
                    ~output_buffer_index.O], \
                    ((out_col_index.O == m.bits(memory_width - 1))))

            tb.READ_VALID <= m.bit(1)
            tb.STENCIL_VALID <= m.bit(1)

    return _TransposeBuffer

def TransposeBuffer(word_width, memory_width, range_, stride, stencil_height):
    return DefineTransposeBuffer(word_width, memory_width, range_, stride, stencil_height)

memory_width = 4
a = TransposeBuffer(1, 4, 1, 1, 4)
tester = fault.Tester(a, a.CLK)
tester.circuit.CLK = 0
tester.circuit.ASYNCRESET = 0
tester.circuit.RESET = 0
b = [1,1,1,0]
c = BitVector[4](0xf)
tester.step(2)
tester.circuit.ASYNCRESET = 1
tester.circuit.RESET = 1
tester.step(2)
tester.circuit.ASYNCRESET = 0
tester.circuit.RESET = 0
tester.step(2)
tester.circuit.SRAM_INPUT=b
tester.circuit.VALID_INPUT = c
tester.eval()
tester.step(2)
for i in range(2 * memory_width +  1):
        tester.circuit.SRAM_INPUT = b
        tester.circuit.VALID_INPUT = c
        tester.eval()
        tester.step(2)

tester.compile_and_run("verilator", flags=["-Wno-fatal", "--trace"], directory="tb")

