dump -file cgra.fsdb -type FSDB
dump -add coord_hold_tb -fsdb_opt +mda+packedmda+struct
power coord_hold_tb.dut
power -enable
run
power -disable
run
exit