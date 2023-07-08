from lake.modules.intersect import *
import magma as m
from magma import *
import tempfile
import kratos as k


def test_iter_basic():
    dut = Intersect(data_width=16,
                    use_merger=False,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)
                    # data_width=16,
                    # use_merger=False,
                    # fifo_depth=2,
                    # defer_fifos=True,
                    # add_flush=False,
                    # perf_debug=perf_debug
    magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)

test_iter_basic()
