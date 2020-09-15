from kratos import *
from math import log


class Memory(Generator):
    def __init__(self,
                 capacity,
                 word_width,
                 num_read_ports,
                 read_port_width,
                 num_write_ports,
                 write_port_width,
                 num_read_write_ports,
                 read_write_port_width,
                 num_banks,
                 chaining,
                 read_write_info,
                 write_info,
                 read_info):

        super().__init__("mem", debug=True)

        write_width = max(write_port_width, read_write_port_width)
        read_width = max(read_port_width, read_write_port_width)

        mem_width = max(write_width, read_width)

        # inputs
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)

        self.data_in = self.input("data_in",
                                  width=word_width,
                                  size=(num_banks, write_width),
                                  explicit_array=True,
                                  packed=True)
        
        self.write = self.input("write", 
                                width=1) 

        self.read = self.input("read",
                               width=1)

        if read_write_info is not None:
            self.read_write_addr = self.input("read_write_addr", clog2(mem_width))
        if read_info is not None:
            self.read_addr = self.input("read_addr", clog2(mem_width))
        if write_info is not None:
            self.write_addr = self.input("write_addr", clog2(mem_width))

        self.data_out = self.output("data_out",
                                    width=word_width,
                                    size=(num_banks, read_width),
                                    explicit_array=True,
                                    packed=True)

        self.memory = self.var("memory",
                               width=word_width,
                               size=(num_banks, mem_width),
                               explicit_array=True,
                               packed=True)

        if read_write_info is not None:
            if read_write_info["latency"] == 1:

        if write_info is not None:
        self.add_code(self.output_data)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def write_data_latency_1(self, idx):
        if ~rst_n:
            for bank in range(num_banks):
                for port in range(mem_width):
                    self.memory = 0

        elif self.write:
            self.memory[self.write_addr_bank][self.write_addr_port] = self.data_in

    @always_comb
    def write_data_latency_0(self):
        for bank in range(num_banks):
            for port in range(write_port_width):
                self.memory[bank][port] = self.data_in[bank][port]

    
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def read_data_latency_1(self):
        self.memory

    @always_comb
    def read_data_latency_0(self):
        for bank in range(num_banks):
            for port in range(read_port_width):
                self.data_out[bank][port] = self.memory[self.read_addr][port]

    @always_comb
    def split_addr(self):
        self.write_addr_bank = self.write_addr[self.write_addr.width - 1, clog2(write_port_width)]
        self.write_addr_port = self.write_addr[clog2(write_port_width) - 1, 0]


if __name__ == "__main__":
    db_dut = Aggregator(word_width=16,
                        mem_word_width=4)
    verilog(db_dut, filename="aggregator.sv")
