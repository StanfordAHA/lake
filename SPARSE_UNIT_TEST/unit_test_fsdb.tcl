dump -file cgra.fsdb -type FSDB
dump -add repeatsig_tb.sv -fsdb_opt +mda+packedmda+struct
power repeatsig_tb.sv.dut
power -enable
run
power -disable
run
exit