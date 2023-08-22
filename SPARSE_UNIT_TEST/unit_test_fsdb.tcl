dump -file cgra.fsdb -type FSDB
dump -add unioner_tb -fsdb_opt +mda+packedmda+struct
power unioner_tb.dut
power -enable
run
power -disable
run
exit