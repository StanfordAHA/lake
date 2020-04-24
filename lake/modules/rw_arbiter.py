from kratos import *
import kratos as kts


def create_port_pkt(data_width,
                    consumer_ports):
    return PackedStruct(f"port_pkt_{data_width}_{consumer_ports}",
                        [("data", data_width, False),
                         ("port", consumer_ports, False),
                         ("valid", 1, False)])


class RWArbiter(Generator):
    '''
    The read_write arbiter dynamically schedules reads and writes to
    a single bank of SRAM
    '''
    def __init__(self,
                 fetch_width=16,
                 data_width=16,
                 memory_depth=32,
                 num_tiles=1,
                 int_in_ports=2,
                 int_out_ports=2,
                 strg_wr_ports=2,
                 strg_rd_ports=2,
                 read_delay=0,
                 rw_same_cycle=True,
                 separate_addresses=True):

        assert not (memory_depth & (memory_depth - 1)), "Memory depth needs to be a power of 2"

        super().__init__("rw_arbiter")
        # Absorb inputs
        self.fetch_width = fetch_width
        self.data_width = data_width
        self.fw_int = int(self.fetch_width / self.data_width)
        self.int_in_ports = int_in_ports
        self.int_out_ports = int_out_ports
        self.strg_wr_ports = strg_wr_ports
        self.strg_rd_ports = strg_rd_ports
        self.memory_depth = memory_depth
        self.num_tiles = num_tiles
        self.mem_addr_width = clog2(self.num_tiles * self.memory_depth)
        self.read_delay = read_delay
        self.rw_same_cycle = rw_same_cycle
        self.separate_addresses = separate_addresses

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Generate the packed struct of the right size
        port_pkt_struct = create_port_pkt(self.fetch_width, self.int_out_ports)

        # Inputs
        self._wen_in = self.input("wen_in", self.strg_wr_ports)
        # self._wen_en = self.input("wen_en", self.strg_wr_ports)
        self._w_data = self.input("w_data",
                                  self.data_width,
                                  size=(self.strg_wr_ports,
                                        self.fw_int),
                                  explicit_array=True,
                                  packed=True)
        self._w_addr = self.input("w_addr", self.mem_addr_width,
                                  size=self.strg_wr_ports,
                                  explicit_array=True,
                                  packed=True)

        self._data_from_mem = self.input("data_from_mem",
                                         self.data_width,
                                         size=(self.strg_rd_ports,
                                               self.fw_int),
                                         explicit_array=True,
                                         packed=True)

        self._mem_valid_data = self.input("mem_valid_data",
                                          self.strg_rd_ports)
        self._out_mem_valid_data = self.output("out_mem_valid_data",
                                               self.strg_rd_ports)

        self._ren_in = self.input("ren_in", self.int_out_ports)
        self._ren_en = self.input("ren_en", self.int_out_ports)
        self._rd_addr = self.input("rd_addr",
                                   self.mem_addr_width,
                                   size=self.int_out_ports,
                                   explicit_array=True,
                                   packed=True)

        self._rd_addr_sel = self.var("rd_addr_sel",
                                     self.mem_addr_width,
                                     size=self.strg_rd_ports,
                                     explicit_array=True,
                                     packed=True)

        # Outputs
        self._out_data = self.output("out_data",
                                     self.data_width,
                                     size=(self.strg_rd_ports,
                                           self.fw_int),
                                     explicit_array=True,
                                     packed=True)
        self._out_port = self.output("out_port", self.int_out_ports,
                                     size=self.strg_rd_ports,
                                     explicit_array=True,
                                     packed=True)
        self._out_valid = self.output("out_valid", self.strg_rd_ports)

        self._cen_mem = self.output("cen_mem", self.strg_rd_ports)
        self._wen_mem = self.output("wen_mem", self.strg_wr_ports)
        self._data_to_mem = self.output("data_to_mem",
                                        self.data_width,
                                        size=(self.strg_wr_ports,
                                              self.fw_int),
                                        explicit_array=True,
                                        packed=True)
        # In this case, need separate addresses
        if self.separate_addresses:
            self._wr_addr_to_mem = self.output("wr_addr_to_mem",
                                               self.mem_addr_width,
                                               size=self.strg_wr_ports,
                                               explicit_array=True,
                                               packed=True)
            self._rd_addr_to_mem = self.output("rd_addr_to_mem",
                                               self.mem_addr_width,
                                               size=self.strg_rd_ports,
                                               explicit_array=True,
                                               packed=True)
        # If the addresses are combined, we better have in==out
        else:
            assert self.strg_rd_ports == self.strg_wr_ports, \
                "Cannot have coalesced address with mismatch port count"
            assert not self.rw_same_cycle, \
                "Cannot read and write with a shared address...set rw_same_cycle to false"
            self._addr_to_mem = self.output("addr_to_mem", self.mem_addr_width,
                                            size=self.strg_rd_ports,
                                            explicit_array=True,
                                            packed=True)

        self._out_ack = self.output("out_ack", self.int_out_ports)

        # Local
        # self._rd_data = self.var("rd_data", self.fetch_width)
        self._wen_int = self.var("wen_int", self.strg_wr_ports)
        self._ren_int = self.var("ren_int", self.int_out_ports)
        self.wire(self._ren_int, self._ren_in & self._ren_en)
        self.wire(self._wen_int, self._wen_in)  # & self._wen_en)

        self._rd_valid = self.var("rd_valid", self.strg_rd_ports)
        self._rd_port = self.var("rd_port", self.int_out_ports,
                                 size=self.strg_rd_ports,
                                 explicit_array=True,
                                 packed=True)
        self._next_rd_port = self.var("next_rd_port", self.int_out_ports,
                                      size=self.strg_rd_ports,
                                      explicit_array=True,
                                      packed=True)

        # For demuxing the read ports
        self._done = self.var("done", self.strg_rd_ports)
        self.add_code(self.set_next_read_port_lowest)
        if(self.strg_rd_ports > 1):
            self._idx_cnt = self.var("idx_cnt", 5,
                                     size=self.strg_rd_ports - 1,
                                     explicit_array=True,
                                     packed=True)
            for i in range(self.strg_rd_ports - 1):
                self.add_code(self.set_next_read_port_alt, index=i + 1)

        # If we have more than one read port, we need to use slightly different logic
        # to set the other reads...

        self._next_rd_port_red = self.var("next_rd_port_red", self.int_out_ports)
        for i in range(self.int_out_ports):
            temp_port = self._next_rd_port[0][i]
            for j in range(self.strg_rd_ports - 1):
                temp_port = kts.concat(temp_port, self._next_rd_port[j + 1][i])
            self.wire(self._next_rd_port_red[i], temp_port.r_or())

        # The next read port can be used to acknowledge reads
        # We do not need to gate the ack if we can read and write in the same cycle
        if self.rw_same_cycle:
            self.wire(self._out_ack,
                      self._next_rd_port_red)
        else:
            self.wire(self._out_ack,
                      self._next_rd_port_red & kts.concat(*([~self._wen_int] * self._out_ack.width)))

        # self.add_code(self.mem_controls)
        if self.separate_addresses:
            for i in range(self.strg_wr_ports):
                self.add_code(self.mem_controls_wr, idx=i)
            for i in range(self.strg_rd_ports):
                self.add_code(self.mem_controls_rd, idx=i)
        else:
            for i in range(self.strg_rd_ports):
                self.add_code(self.mem_controls_combined, idx=i)

        if self.read_delay == 1:
            for i in range(self.strg_rd_ports):
                self.add_code(self.next_read_valid, idx=i)
        else:
            for i in range(self.strg_rd_ports):
                self.add_code(self.zero_delay_read, idx=i)
        self.add_code(self.output_stage)

    @always_comb
    def mem_controls_wr(self, idx):
        self._wen_mem[idx] = self._wen_int[idx]
        self._data_to_mem[idx] = self._w_data[idx]
        self._wr_addr_to_mem[idx] = self._w_addr[idx]

    @always_comb
    def mem_controls_rd(self, idx):
        # cen_mem acts as ren_mem when the ports are separated
        self._cen_mem[idx] = self._next_rd_port[idx].r_or()
        self._rd_addr_to_mem[idx] = self._rd_addr_sel[idx]

    @always_comb
    # Prioritizes writes over reads
    def mem_controls_combined(self, idx):
        self._wen_mem[idx] = self._wen_int[idx]
        self._cen_mem[idx] = (self._wen_int[idx] | (self._next_rd_port[idx].r_or()))
        self._data_to_mem[idx] = self._w_data[idx]
        # Consume wr over read
        if(self._wen_int[idx]):
            self._addr_to_mem[idx] = self._w_addr[idx]
        else:
            self._addr_to_mem[idx] = self._rd_addr_sel[idx]

    @always_comb
    # Find lowest ready
    def set_next_read_port_lowest(self):
        self._next_rd_port[0] = 0
        self._rd_addr_sel[0] = 0
        self._done[0] = 0
        for i in range(self.int_out_ports):
            if ~self._done[0]:
                if self._ren_int[i]:
                    self._rd_addr_sel[0] = self._rd_addr[i]
                    self._next_rd_port[0][i] = 1
                    self._done[0] = 1

    # Find lowest ready
    @always_comb
    def set_next_read_port_alt(self, index):
        self._next_rd_port[index] = 0
        self._idx_cnt[index - 1] = 0
        self._rd_addr_sel[index] = 0
        self._done[index] = 0
        for i in range(self.int_out_ports):
            if ~self._done[index]:
                if self._ren_int[i] & (self._idx_cnt[index - 1] == index):
                    self._done[index] = 1
                    self._rd_addr_sel[index] = self._rd_addr[i]
                    self._next_rd_port[index][i] = 1
                self._idx_cnt[index - 1] = self._idx_cnt[index - 1] + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def next_read_valid(self, idx):
        if ~self._rst_n:
            self._rd_port[idx] = 0
            self._rd_valid[idx] = 0
        else:
            self._rd_valid[idx] = ((~self._wen_int[idx] | (self.rw_same_cycle)) &
                                   (self._next_rd_port[idx].r_or()))
            self._rd_port[idx] = self._next_rd_port[idx]

    @always_comb
    def zero_delay_read(self, idx):
        self._rd_valid[idx] = (~self._wen_int[idx] | (self.rw_same_cycle)) & (self._next_rd_port[idx].r_or())
        self._rd_port[idx] = self._next_rd_port[idx]

    @always_comb
    def output_stage(self):
        self._out_data = self._data_from_mem
        self._out_port = self._rd_port
        self._out_valid = self._rd_valid
        self._out_mem_valid_data = self._mem_valid_data


if __name__ == "__main__":
    db_dut = RWArbiter()

    verilog(db_dut, filename="rw_arbiter.sv")
