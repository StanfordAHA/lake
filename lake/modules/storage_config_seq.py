from kratos import *
from lake.modules.aggregator import Aggregator
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
import kratos as kts


class StorageConfigSeq(Generator):
    '''
    Sequence the reads and writes to the storage unit - if dealing with
    a storage unit that has multiple r/w, should only use one of the ports

    If the storage unit is wider than the data, this sequencer expects all
    X writes/reads in order will come from the same word
    '''
    def __init__(self,
                 data_width,
                 config_addr_width,
                 addr_width,
                 fetch_width,
                 total_sets,
                 sets_per_macro):
        super().__init__("storage_config_seq")

        self.data_width = data_width
        self.config_addr_width = config_addr_width
        self.addr_width = addr_width
        self.fetch_width = fetch_width
        self.fw_int = int(self.fetch_width / self.data_width)
        self.total_sets = total_sets
        self.sets_per_macro = sets_per_macro
        self.banks = int(self.total_sets / self.sets_per_macro)

        self.set_addr_width = clog2(total_sets)

        # self.storage_addr_width = self.

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        # phases = [] TODO
        # Take in the valid and data and attach an address + direct to a port
        self._config_data_in = self.input("config_data_in",
                                          self.data_width)

        self._config_addr_in = self.input("config_addr_in",
                                          self.config_addr_width)

        self._config_wr = self.input("config_wr", 1)
        self._config_rd = self.input("config_rd", 1)
        self._config_en = self.input("config_en", self.total_sets)

        self._clk_en = self.input("clk_en", 1)

        self._rd_data_stg = self.input("rd_data_stg", self.data_width,
                                       size=(self.banks,
                                             self.fw_int),
                                       explicit_array=True,
                                       packed=True)

        self._wr_data = self.output("wr_data",
                                    self.data_width,
                                    size=self.fw_int,
                                    explicit_array=True,
                                    packed=True)

        self._rd_data_out = self.output("rd_data_out", self.data_width,
                                        size=self.total_sets,
                                        explicit_array=True,
                                        packed=True)

        self._addr_out = self.output("addr_out",
                                     self.addr_width)

        # One set per macro means we directly send the config address through
        if self.sets_per_macro == 1:
            width = self.addr_width - self.config_addr_width
            if width > 0:
                self.wire(self._addr_out, kts.concat(kts.const(0, width), self._config_addr_in))
            else:
                self.wire(self._addr_out, self._config_addr_in[self.addr_width - 1, 0])
        else:
            width = self.addr_width - self.config_addr_width - clog2(self.sets_per_macro)
            self._set_to_addr = self.var("set_to_addr",
                                         clog2(self.sets_per_macro))
            self._reduce_en = self.var("reduce_en", self.sets_per_macro)
            for i in range(self.sets_per_macro):
                reduce_var = self._config_en[i]
                for j in range(self.banks - 1):
                    reduce_var = kts.concat(reduce_var, self._config_en[i + (self.sets_per_macro * (j + 1))])
                self.wire(self._reduce_en[i], reduce_var.r_or())
            self.add_code(self.demux_set_addr)
            if width > 0:
                self.wire(self._addr_out, kts.concat(kts.const(0, width),
                          self._set_to_addr,
                          self._config_addr_in))
            else:
                self.wire(self._addr_out, kts.concat(self._set_to_addr, self._config_addr_in))

        self._wen_out = self.output("wen_out", self.banks)
        self._ren_out = self.output("ren_out", self.banks)

        # Handle data passing
        if self.fw_int == 1:
            # If word width is same as data width, just pass everything through
            self.wire(self._wr_data[0], self._config_data_in)
            # self.wire(self._rd_data_out, self._rd_data_stg[0])
            num = 0
            for i in range(self.banks):
                for j in range(self.sets_per_macro):
                    self.wire(self._rd_data_out[num], self._rd_data_stg[i])
                    num = num + 1
        else:
            self._data_wr_reg = self.var("data_wr_reg",
                                         self.data_width,
                                         size=self.fw_int - 1,
                                         packed=True,
                                         explicit_array=True)
            # self._data_rd_reg = self.var("data_rd_reg",
            #                              self.data_width,
            #                              size=self.fw_int - 1,
            #                              packed=True,
            #                              explicit_array=True)

            # Have word counter for repeated reads/writes
            self._cnt = self.var("cnt", clog2(self.fw_int))
            self._rd_cnt = self.var("rd_cnt", clog2(self.fw_int))
            self.add_code(self.update_cnt)
            self.add_code(self.update_rd_cnt)
            # Gate wen if not about to finish the word

            num = 0
            for i in range(self.banks):
                for j in range(self.sets_per_macro):
                    self.wire(self._rd_data_out[num], self._rd_data_stg[i][self._rd_cnt])
                    num = num + 1

            # Deal with writing to the data buffer
            self.add_code(self.write_buffer)

            # Wire the reg + such to this guy
            for i in range(self.fw_int - 1):
                self.wire(self._wr_data[i], self._data_wr_reg[i])
            self.wire(self._wr_data[self.fw_int - 1], self._config_data_in)

        # If we have one bank, we can just always rd/wr from that one
        if self.banks == 1:
            if self.fw_int == 1:
                self.wire(self._wen_out, self._config_wr)
            else:
                self.wire(self._wen_out,
                          self._config_wr & (self._cnt == (self.fw_int - 1)))
            self.wire(self._ren_out, self._config_rd)
        # Otherwise we need to extract the bank from the set
        else:
            if self.fw_int == 1:
                for i in range(self.banks):
                    width = self.sets_per_macro
                    self.wire(self._wen_out[i], self._config_wr &
                              self._config_en[(i + 1) * width - 1, i * width].r_or())
            else:
                for i in range(self.banks):
                    width = self.sets_per_macro
                    self.wire(self._wen_out[i],
                              self._config_wr & self._config_en[(i + 1) * width - 1, i * width].r_or() &
                              (self._cnt == (self.fw_int - 1)))

            for i in range(self.banks):
                width = self.sets_per_macro
                self.wire(self._ren_out[i],
                          self._config_rd & self._config_en[(i + 1) * width - 1, i * width].r_or())

    @always_comb
    def demux_set_addr(self):
        self._set_to_addr = 0
        for i in range(self.sets_per_macro):
            if self._reduce_en[i]:
                self._set_to_addr = i

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_cnt(self):
        if ~self._rst_n:
            self._cnt = 0
        # Increment when reading/writing - making sure
        # that the sequencing is correct from app level!
        elif (self._config_wr | self._config_rd) & self._config_en.r_or():
            self._cnt = self._cnt + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_rd_cnt(self):
        if ~self._rst_n:
            self._rd_cnt = 0
        # Increment when reading/writing - making sure
        # that the sequencing is correct from app level!
        else:
            self._rd_cnt = self._cnt

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def write_buffer(self):
        if ~self._rst_n:
            self._data_wr_reg = 0
        # Increment when reading/writing - making sure
        # that the sequencing is correct from app level!
        elif self._config_wr & (self._cnt < self.fw_int - 1):
            self._data_wr_reg[self._cnt] = self._config_data_in


if __name__ == "__main__":
    db_dut = StorageConfigSeq(data_width=16,
                              config_addr_width=8,
                              addr_width=16,
                              fetch_width=64,
                              total_sets=2,
                              sets_per_macro=2)
    verilog(db_dut, filename="storage_config_seq.sv",
            additional_passes={"lift config regs": lift_config_reg})
