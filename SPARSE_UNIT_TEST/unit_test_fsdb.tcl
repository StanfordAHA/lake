dump -file cgra.fsdb -type FSDB
dump -add fiber_glb2fib_val_tb -fsdb_opt +mda+packedmda+struct
power fiber_glb2fib_val_tb.dut
power -enable
run
power -disable
run
exit