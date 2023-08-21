dump -file cgra.fsdb -type FSDB
dump -add coord_drop_tb -fsdb_opt +mda+packedmda+struct
power coord_drop_tb.dut
power -enable
run
power -disable
run
exit