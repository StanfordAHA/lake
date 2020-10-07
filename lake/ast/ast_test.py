
import math as m

def affine_map(starting_addr = 0,
               range_0 = 10,
               range_1 = 10,
               stride_0 = 1,
               stride_1 = 1):
    for y in range(range_1):
        for x in range(range_0):
            out = x * stride_0 + y * stride_1 + starting_addr