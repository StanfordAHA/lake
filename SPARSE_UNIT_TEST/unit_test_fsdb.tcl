dump -file cgra.fsdb -type FSDB
dump -add intersect_tb -fsdb_opt +mda+packedmda+struct
power intersect_tb.dut
power -enable
run
power -disable
run
exit