dump -file cgra.fsdb -type FSDB
dump -add pe_tb -fsdb_opt +mda+packedmda+struct
power pe_tb.dut
power -enable
run
power -disable
run
exit