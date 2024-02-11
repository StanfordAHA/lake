dump -file cgra.fsdb -type FSDB
dump -add value_tb -fsdb_opt +mda+packedmda+struct
power value_tb.dut
power -enable
run
power -disable
run
exit