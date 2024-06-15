dump -file cgra.fsdb -type FSDB
dump -add pass_through_tb -fsdb_opt +mda+packedmda+struct
power pass_through_tb.dut
power -enable
run
power -disable
run
exit