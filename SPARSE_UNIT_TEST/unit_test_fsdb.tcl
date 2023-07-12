dump -file cgra.fsdb -type FSDB
dump -add unioner_tb.sv -fsdb_opt +mda+packedmda+struct
power unioner_tb.sv.dut
power -enable
run
power -disable
run
exit