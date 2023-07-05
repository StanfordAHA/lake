from lake.modules.intersect import *
import magma as m
from magma import *
import tempfile
import kratos as k


dut = Intersect(data_width=16,
                use_merger=False,
                defer_fifos=False,
                add_flush=True)
magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
