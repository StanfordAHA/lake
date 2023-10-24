dump -file cgra.fsdb -type FSDB
dump -add fiber_access_tb -fsdb_opt +mda+packedmda+struct
power fiber_access_tb.dut
power -enable
run
power -disable
run
exit