dump -file cgra.fsdb -type FSDB
dump -add multi_flow_2h_tb -fsdb_opt +mda+packedmda+struct
power multi_flow_2h_tb.dut
power -enable
run
power -disable
run
exit