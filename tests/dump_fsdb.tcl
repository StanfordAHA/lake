dump -file mu2f_io_core.fsdb -type FSDB
dump -add mu2f_io_core_tb -fsdb_opt +mda+packedmda+struct
power mu2f_io_core_tb.dut
power -enable
run
power -disable
run