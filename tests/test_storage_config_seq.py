from logging import lastResort
from lake.models.sram_model import SRAMModel
from lake.modules.storage_config_seq_tb import StorageConfigSeqTb, get_db_dut
import magma as m
from magma import *
import tempfile
import kratos as k
import random as rand
import fault


# tests only 1 bank
def test_storage_config_seq(data_width=16,      # CGRA Params
                            mem_width=64,
                            mem_depth=512,
                            test_cases=1000,   # numbers of cycles excluding axi gap cycles
                            axi_gap_min=10,      # MUST be > 0, ~1000 in Garnet
                            axi_gap_max=20,     # ~1200 in Garnet
                            axi_rd_timing=2     # numbers of cycles for 1 read
                            ):

    num_words_per_row = int(mem_width / data_width)

    # Set up model...
    config_addr_width = 8
    data_words_per_set = 2 ** config_addr_width
    sets_per_macro = max(1, int(mem_depth / data_words_per_set))
    sram_model = []
    for i in range(sets_per_macro):
        sram_model.append([])  # new set
        for j in range(data_words_per_set):
            sram_model[i].append([])  # new row
            for x in range (num_words_per_row):
                sram_model[i][j].append(0)  # new word
    ###

    # Set up dut...
    db_dut, need_config_lift, use_sram_stub, tsmc_info \
        = get_db_dut(data_width=data_width,
                     mem_width=mem_width,
                     mem_depth=mem_depth)
    magma_dut = k.util.to_magma(db_dut,
                                optimize_if=False,
                                flatten_array=True)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    # default values
    tester.circuit.flush = 0
    tester.circuit.tile_en = 1
    tester.circuit.clk_en = 1

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    rand.seed(0)

    # loop variables
    test_num = 0
    write_cnt = num_words_per_row
    read_cnt = num_words_per_row
    written_addr_cfg = []  # each element has [addr, config_en]
    last_read_addr = 0
    last_read_en = 0
    last_read_config_en = 0
    last_model_rd_reg = 0

    while test_num < test_cases or \
            (write_cnt < num_words_per_row - 1) or \
            (read_cnt < num_words_per_row - 1):

        # generating test inputs
        # either read or write
        # does not make too much sense for read+write to the same addr
        config_data_in = rand.randint(0, 2 ** db_dut.data_width - 1)

        # need consecutive writes to update the entire mem_width
        if write_cnt < num_words_per_row - 1:
            # config_en same
            # config_addr_in same
            config_write = 1
            write_cnt += 1
        # need consecutive reads to retreive the entire mem_width
        elif read_cnt < num_words_per_row - 1:
            # config_en same
            # config_addr_in same
            config_read = 1
            read_cnt += 1
        else:
            # new read/write request
            # config_en is 1-hot
            config_en = rand.randint(0, 1) * (2 ** rand.randint(0, db_dut.total_sets - 1))
            config_addr_in = rand.randint(0, 2 ** config_addr_width - 1)
            config_write = rand.randint(0, 1)
            config_read = 1 - config_write

            if (config_write == 1) and (config_en > 0):
                write_cnt = 0
                # stores all the written addresses
                # they would be readout at the end of test
                # contains duplicated addresses
                written_addr_cfg.append([config_addr_in, config_en])

            if (config_read == 1) and (config_en > 0):
                read_cnt = 0

        # update dut
        tester.circuit.clk_en = config_en > 0
        tester.circuit.config_en = config_en
        tester.circuit.config_read = config_read
        tester.circuit.config_write = config_write
        tester.circuit.config_addr_in = config_addr_in
        tester.circuit.config_data_in = config_data_in

        # simualte the actual AXI read which asserts config_en and config_rd/config_wr
        # for more than 1 cycles to meet timing
        if config_en > 0 and config_read == 1:
            for i in range (axi_rd_timing - 1):
                tester.eval()
                tester.step(2)

        # update the model
        if config_en > 0 and config_write == 1:
            # decoding the set from 1-hot config_en
            for i in range (sets_per_macro - 1, -1, -1):
                if config_en / (2**i) >= 1:
                    sram_model[i][config_addr_in][write_cnt] = config_data_in
                    break

        tester.eval()

        # remember the last states for consecutive read/write
        if last_read_en:
            tester.circuit.config_data_out_0.expect(last_model_rd_reg)
            tester.circuit.config_data_out_1.expect(last_model_rd_reg)

        # remember the model read data to
        # check against the circuit output in the next cycle
        if config_en > 0 and config_read == 1:
            last_read_en = 1

            # computes the enabled set from the 1-hot encoding
            for i in range (sets_per_macro - 1, -1, -1):
                if config_en / (2 ** i) >= 1:
                    last_model_rd_reg = sram_model[i][config_addr_in][read_cnt]
                    break
        else:
            last_read_en = 0

        tester.step(2)

        # inserting bubbles in consecutive reads/writes
        if config_en > 0 and config_read == 1:
            stall = rand.randint(axi_gap_min, axi_gap_max)
        else:
            # for write or nop, the min stall is 0 meaning can be back-to-back
            stall = rand.randint(0, axi_gap_max)
        if stall > 0:
            # needs to check circuit read data before it becomes invalid
            if last_read_en:
                tester.circuit.config_data_out_0.expect(last_model_rd_reg)
                tester.circuit.config_data_out_1.expect(last_model_rd_reg)
            last_read_en = 0

            tester.circuit.clk_en = 0
            tester.circuit.config_en = 0
            # optional to clear config_write and config_read
            tester.circuit.config_write = 0
            tester.circuit.config_read = 0
            for i in range(stall):
                tester.eval()
                tester.step(2)

        test_num += 1

    # compare all the written data
    tester.circuit.config_read = 1
    tester.circuit.config_write = 0
    tester.circuit.config_data_in = 0
    for addr_cfg in written_addr_cfg:
        addr = addr_cfg[0]
        config_en = addr_cfg[1]

        set = 0
        # computes the enabled set from the 1-hot encoding
        for i in range (sets_per_macro - 1, -1, -1):
            if config_en / (2 ** i) >= 1:
                set = i
                break

        # retrieve from model
        for word in sram_model[set][addr]:
            # retrieve from dut
            tester.circuit.clk_en = config_en > 0
            tester.circuit.config_en = config_en
            tester.circuit.config_addr_in = addr

            # simualte the actual AXI read which asserts cfg_en and cfg_rd
            # for more than 1 cycles to meet timing
            for i in range (axi_rd_timing - 1):
                tester.eval()
                tester.step(2)

            # needs to wait 1 cycle for read data
            tester.eval()
            tester.step(2)

            tester.circuit.config_data_out_0.expect(word)
            tester.circuit.config_data_out_1.expect(word)

            # inserting simualted axi wait cycles in consecutive reads/writes
            stall = rand.randint(axi_gap_min, axi_gap_max)
            if stall > 0:
                tester.circuit.clk_en = 0
                tester.circuit.config_en = 0
                # optional to clear config_write and config_read
                tester.circuit.config_write = 0
                tester.circuit.config_read = 0
                for i in range(stall):
                    tester.eval()
                    tester.step(2)

            # restore config_en
            tester.circuit.clk_en = config_en > 0
            tester.circuit.config_en = config_en
            tester.circuit.config_read = 1

    with tempfile.TemporaryDirectory() as tempdir:
        # The Xcelium run will fail when we could be reading from non-initialized addresses
        # and the received data will be XXXX
        # while Verilator will not fail when comparing XXXX
        # tester.compile_and_run(target="system-verilog",
        #                        simulator="xcelium",
        #                        directory="tempdir",
        #                        magma_output="verilog",
        #                        dump_waveforms=True,
        #                        flags=["-sv"])
        tester.compile_and_run(target="verilator",
                               directory="tempdir",
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_storage_config_seq()
