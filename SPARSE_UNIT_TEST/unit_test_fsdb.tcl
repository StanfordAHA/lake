dump -file cgra.fsdb -type FSDB
dump -add stream_arbiter_tb -fsdb_opt +mda+packedmda+struct
power stream_arbiter_tb.dut
power -enable
run
power -disable
run
exit