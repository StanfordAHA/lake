dump -file cgra.fsdb -type FSDB
dump -add repeat_tb -fsdb_opt +mda+packedmda+struct
power repeat_tb.dut
power -enable
run
power -disable
run
exit