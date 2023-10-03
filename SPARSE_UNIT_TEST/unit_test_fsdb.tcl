dump -file cgra.fsdb -type FSDB
dump -add sam_dense_tb -fsdb_opt +mda+packedmda+struct
power sam_dense_tb.dut
power -enable
run
power -disable
run
exit