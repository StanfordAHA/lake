from kratos import *
from math import log
from collateral2compiler import port


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
                                  size=(num_ports, write_width),
                                  explicit_array=True,
                                  packed=True)
        
        self.write = self.input("write", 
                                width=1) 

        self.read = self.input("read",
                               width=1)

        if read_write_info is not None:
            self.read_write_addr = self.input("read_write_addr", clog2(mem_width))

        self.data_out = self.output("data_out",
                                    width=word_width,
                                    size=(num_ports, read_width),
                                    explicit_array=True,
                                    packed=True)

        self.memory = self.var("memory",
                               width=word_width,
                               size=(num_banks, mem_width),
                               explicit_array=True,
                               packed=True)

        if read_write_info is not None:
            if read_write_info["latency"] == 1:

        split_addr_signals = False
        if write_info is not None:
            self.write_addr = self.input("write_addr", clog2(mem_width))
            if write_info["latency"] == 1:
                self.write_addr_bank = self.var("write_addr_bank",
                                                self.write_addr.width - clog2(write_port_width))
                self.write_addr_port = self.var("write_addr_port", clog2(write_port_width))
                self.add_code(write_data_latency_1)
                split_addr_signals = True
            else:
                self.add_code(write_data_latency_0)

        if read_info is not None:
            self.read_addr = self.input("read_addr", clog2(mem_width)) 
            if read_info["latency"] == 1:
                self.add_code(read_data_latency_1)
            else:
                self.read_addr_bank = self.var("read_addr_bank",
                                               clog2(num_banks) + clog2(read_port_width) - clog2(read_port_width))
                self.read_addr_port = self.var("read_addr_port", clog2(read_port_width))
                self.add_code(read_data_latency_0)
                split_addr_signals = True

        if split_addr_signals:
            self.add_code(self.split_addr)

        self.add_code(self.output_data)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def write_data_latency_1(self):
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

    
    #@always_ff((posedge, "clk"), (negedge, "rst_n"))
    #def read_data_latency_1(self):
    #    self.memory

    @always_comb
    def read_data_latency_0(self):
        for bank in range(num_banks):
            for port in range(read_port_width):
                self.data_out[bank][port] = self.memory[self.read_addr_bank][self.read_addr_port]

    @always_comb
    def split_addr(self):
        self.write_addr_bank = self.write_addr[self.write_addr.width - 1, clog2(write_port_width)]
        self.write_addr_port = self.write_addr[clog2(write_port_width) - 1, 0]
        self.read_addr_bank = self.read_addr[clog2(num_banks) + clog2(read_port_width) - 1, clog2(read_port_width)]
        self.read_addr_port = self.read_addr[clog2(read_port_width) - 1, 0]


if __name__ == "__main__":
    Memory(
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
