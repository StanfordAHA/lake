from kratos import *
from math import log
from lake.collateral2compiler.mem_port import MemPort


class Memory(Generator):
    def __init__(self,
                 capacity,
                 word_width,
                 num_read_ports,
                 read_port_width,
                 num_write_ports,
                 write_port_width,
                 # num_read_write_ports,
                 # read_write_port_width,
                 chaining,
                 # read_write_info,
                 write_info,
                 read_info):

        super().__init__("mem", debug=True)

        self.capacity = capacity
        self.word_width = word_width
        self.num_read_ports = num_read_ports

        write_width = write_port_width #max(write_port_width, read_write_port_width)
        read_width = read_port_width #max(read_port_width, read_write_port_width)

        assert capacity % write_width == 0
        assert capacity % read_width == 0

        write_width_bits = max(1, clog2(write_width))
        read_width_bits = max(1, clog2(read_width))

        mem_width = max(write_width, read_width)

        mem_width_bits = max(1, clog2(mem_width))

        addr_width = max(1, clog2(capacity))

        # inputs
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)

        self.data_in = self.input("data_in",
                                  width=word_width,
                                  size=write_width,
                                  explicit_array=True,
                                  packed=True)
        
        #if read_write_info is not None:
        #    self.read_write_addr = self.input("read_write_addr", clog2(mem_width))

        self.data_out = self.output("data_out",
                                    width=word_width,
                                    size=read_width,
                                    explicit_array=True,
                                    packed=True)

        self.memory = self.var("memory",
                               width=word_width,
                               size=capacity,
                               explicit_array=True,
                               packed=True)

        #if read_write_info is not None:
        #    if read_write_info["latency"] == 1:

        if write_info is not None:
            self.write_addr = self.input("write_addr", 
                                         width=addr_width)

            if write_info["latency"] == 1:
                self.write = self.input("write", 1)
                self.add_code(self.write_data_latency_1)
            else:
                self.add_code(self.write_data_latency_0)

        if read_info is not None:
            self.read_addr = self.input("read_addr",
                                        width=addr_width)

            if read_info["latency"] == 1:
                self.add_code(self.read_data_latency_1)
            else:
                self.add_code(self.read_data_latency_0)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def write_data_latency_1(self):
        if ~self.rst_n:
            for addr in range(self.capacity):
                self.memory[addr] = 0

        elif self.write:
            #if write_port_width == 1:
            #    self.memory[0][self.write_addr] = self.data_in[0]
            #else:
            for w in range(write_port_width):
                self.memory[self.write_addr + w] = self.data_in[w]

    #@always_comb
    #def write_data_latency_0(self):
    #    self.memory = 0
    #    for bank in range(num_banks):
    #        for port in range(write_port_width):
    #            self.memory[bank][port] = self.data_in[bank][port]

    
    #@always_ff((posedge, "clk"), (negedge, "rst_n"))
    #def read_data_latency_1(self):
    #    self.memory

    @always_comb
    def read_data_latency_0(self):
        for port in range(read_port_width):
            self.data_out[port] = self.memory[self.read_addr + port]


if __name__ == "__main__":

    write_port = MemPort(1, 0)
    read_port = MemPort(0, 0)
    agg = \
        Memory(capacity=4,
               word_width=16,
               num_read_ports=1,
               read_port_width=4,
               num_write_ports=1,
               write_port_width=1,
               chaining=0,
               write_info=write_port.port_info,
               read_info=read_port.port_info)

    verilog(agg, filename="mem.sv")


    write_port = MemPort(1, 0)
    read_port = MemPort(0, 0)
    tb = \
        Memory(capacity=8,
               word_width=16,
               num_read_ports=1,
               read_port_width=1,
               num_write_ports=1,
               write_port_width=4,
               chaining=0,
               write_info=write_port.port_info,
               read_info=read_port.port_info)

    # verilog(tb, filename="mem.sv")
