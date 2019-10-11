import magma as m
import mantle
import fault
import math

from math import ceil, log
from mantle import Register

def DefineTransposeBuffer(word_width, memory_width, range_, stride, stencil_height):
    class _TransposeBuffer(m.Circuit):
        name = 'Transpose_Buffer'
        IO = [  'RST_N', m.In(m.AsyncReset), \
                'SRAM_INPUT', m.In(m.Array[memory_width, m.Bits[word_width]]), \
                'VALID_INPUT', m.In(m.Array[memory_width, m.Bit]), \
                'VALID_COL_PIXELS', m.Out(m.Array[stencil_height, m.Bits[word_width]]), \
                'READ_VALID', m.Out(m.Bit), \
                'STENCIL_VALID', m.Out(m.Bit)] \
                + m.ClockInterface()

        @classmethod
        def definition(tb):
            output_buffer_index = Register(1)
            read_row_index = Register(ceil(log(stencil_height, 2)))
            read_col_index = Register(ceil(log(memory_width, 2)))
            out_col_index = Register(ceil(log(memory_width, 2)))
            # active low asynchronous reset
            if (not tb.RST_N):
                output_buffer_index.I <= 0
                read_row_index.I <= 0
                read_col_index.I <= 0
                out_col_index.I <= 0

            b = m.join([Register(word_width) for _ in range(memory_width)])
            for i in range(memory_width):
                b.I <= tb.SRAM_INPUT
                
            transpose_buffer = m.join([m.join([m.join([Register(word_width) for _ in range(memory_width)]) for __ in range(stencil_height)]) for ___ in range(2)])
#            transpose_buffer = [[[Register(word_width)  for i in range(memory_width)] \
#                                                        for j in range(stencil_height)] \
#                                                        for k in range(2)] # double buffering
            # store input SRAM data in transpose buffer
            for i in range(len(tb.VALID_INPUT)):
                if (tb.VALID_INPUT[i] == m.bit(1)):
                    transpose_buffer[not output_buffer_index.O][read_row_index.O][read_col_index.O].I <= tb.SRAM_INPUT[i]
                    real_col_index.I <= read_col_index.O + 1

            read_row_index.I <=  0  if (read_row_index.O == stencil_height - 1) \
                                    else (read_row_index.O + 1)
            read_col_index.I <=  0  if (real_col_index.O == memory_width) \
                                    else real_col_index.O

            # output pixels to stencil_height shift registers from transpose buffer
            for i in range(len(tb.VALID_COL_PIXELS)):
                tb.VALID_COL_PIXELS[i] <= transpose_buffer[output_buffer.O][i][out_col_index.O].O

            if (out_col_index.O == memory_width - 1):
                out_col_index.I <= 0
                output_buffer_index.I <= 0 if (output_buffer_index.O == 1) else 1
            else:
                out_col_index.I <= out_col_index.O + 1

            tb.READ_VALID <= m.bit(1)
            tb.STENCIL_VALID <= m.bit(1)

    return _TransposeBuffer

def TransposeBuffer(word_width, memory_width, range_, stride, stencil_height):
    return DefineTransposeBuffer(word_width, memory_width, range_, stride, stencil_height)

a = TransposeBuffer(1, 3, 1, 1, 3)
tester = fault.Tester(a, a.CLK)
tester.reset()
tester.circuit.CLK = 0
tester.step(2)
b = [1, 0, 0, 1, 1, 0, 0, 0]
c = [1 for i in range(8)]
b = m.array(b)
for i in range(2 * memory_width +  1):
        tester.circuit.SRAM_INPUT = b
        tester.circuit.VALID_INPUT = c
        tester.eval()
        tester.step(2)

tester.compile_and_run("verilator", flags=["Wno-fatal", "--trace"], directory="debug")


