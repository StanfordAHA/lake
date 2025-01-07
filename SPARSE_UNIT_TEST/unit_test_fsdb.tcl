dump -file cgra.fsdb -type FSDB
dump -add fiber_fib2glb_seg_tb -fsdb_opt +mda+packedmda+struct
power fiber_fib2glb_seg_tb.dut
power -enable
run
power -disable
run
exit