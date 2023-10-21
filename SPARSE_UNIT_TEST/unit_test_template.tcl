dump -file cgra.fsdb -type FSDB
dump -add xxx -fsdb_opt +mda+packedmda+struct
power xxx.dut
power -enable
run
power -disable
run
exit