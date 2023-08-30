dump -file cgra.fsdb -type FSDB
dump -add unioner_vmode_tb -fsdb_opt +mda+packedmda+struct
power unioner_vmode_tb.dut
power -enable
run
power -disable
run
exit