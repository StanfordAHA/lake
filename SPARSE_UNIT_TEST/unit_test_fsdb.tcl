dump -file cgra.fsdb -type FSDB
dump -add reduce_tb -fsdb_opt +mda+packedmda+struct
power reduce_tb.dut
power -enable
run
power -disable
run
exit