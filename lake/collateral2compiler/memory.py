from kratos import *
from math import log
from lake.collateral2compiler.mem_port import MemPort
from lake.utils.util import safe_wire
from lake.collateral2compiler.helper import *


def mem_inst(mem_params, mem_collateral):

    # default port addr domain is full capacity of memory
    for p in mem_params["write_info"] + mem_params["read_info"]:
        p.set_addr_domain([0, mem_params["capacity"]-1])

    for s in ["write_info", "read_info"]:
        mem_params[s] = [p.port_info for p in mem_params[s]]
    
    mem = Memory(mem_params)
    get_memory_params(mem, mem_collateral)

    return mem


class Memory(Generator):
    def __init__(self,
                 mem_params):
        # capacity,
        # word_width,
        # num_read_ports,
        # read_port_width,
        # num_write_ports,
        # write_port_width,
        #  num_read_write_ports,
        #  read_write_port_width,
        # chaining,
        #  read_write_info,
        # write_info,
        # read_info):

        super().__init__("mem", debug=True)

        print(mem_params)
        self.capacity = mem_params["capacity"]

        self.word_width = mem_params["word_width"]
        self.num_read_ports = mem_params["num_read_ports"]
        self.num_write_ports = mem_params["num_write_ports"]

        self.write_width = mem_params["write_port_width"]  # max(write_port_width, read_write_port_width)
        self.read_width = mem_params["read_port_width"]  # max(read_port_width, read_write_port_width)

        self.write_info = mem_params["write_info"][0]
        self.read_info = mem_params["read_info"][0]

        self.chaining = mem_params["chaining"]

        assert self.capacity % self.write_width == 0
        assert self.capacity % self.read_width == 0

        self.write_width_bits = max(1, clog2(self.write_width))
        self.read_width_bits = max(1, clog2(self.read_width))

        self.mem_width = max(self.write_width, self.read_width)

        self.mem_width_bits = max(1, clog2(self.mem_width))

        self.addr_width = max(1, clog2(self.capacity))

        self.write_bits = clog2((self.capacity / self.write_width))
        self.read_bits = clog2((self.capacity / self.read_width))

        # inputs
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)

        self.data_in = self.input("data_in",
                                  width=self.word_width,
                                  size=self.write_width,
                                  explicit_array=True,
                                  packed=True)

        # if read_write_info is not None:
        #    self.read_write_addr = self.input("read_write_addr", clog2(mem_width))

        self.data_out = self.output("data_out",
                                    width=self.word_width,
                                    size=self.read_width,
                                    explicit_array=True,
                                    packed=True)

        self.memory = self.var("memory",
                               width=self.word_width,
                               size=self.capacity,
                               explicit_array=True,
                               packed=True)

        # if read_write_info is not None:
        #    if read_write_info["latency"] == 1:

        if self.write_info is not None:
            self.write_addr = self.input("write_addr",
                                         width=self.addr_width)
            if self.write_info["latency"] == 1:
                self.write = self.input("write", 1)
                self.add_code(self.write_data_latency_1)
            else:
                self.add_code(self.write_data_latency_0)

        if self.read_info is not None:
            self.read_addr = self.input("read_addr",
                                        width=self.addr_width)
            if self.read_info["latency"] == 1:
                self.add_code(self.read_data_latency_1)
            else:
                self.add_code(self.read_data_latency_0)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def write_data_latency_1(self):
        if self.write:
            for i in range(self.write_width):
                self.memory[self.write_addr + i] = self.data_in[i]

    @always_comb
    def read_data_latency_0(self):
        for port in range(self.read_width):
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

    verilog(tb, filename="mem_tb.sv")
