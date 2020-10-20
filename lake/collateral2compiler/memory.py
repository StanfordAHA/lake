from kratos import *
from math import log
from lake.collateral2compiler.mem_port import MemPort
from lake.utils.util import safe_wire
from lake.collateral2compiler.helper import *


def port_to_info(mem_params):

    port_types = ["write", "read", "read_write"]
    for s_ in port_types:
        s = s_ + "_ports"
        if s not in mem_params.keys():
            mem_params[s] = []

    all_ports = []
    for s_ in port_types:
        all_ports += mem_params[s_ + "_ports"]

    # default port addr domain is full capacity of memory
    for p in all_ports:
        # print(p.port_info["addr_domain"])
        if p.port_info["addr_domain"]["min"] == -1 and p.port_info["addr_domain"]["max"] == -1:
            p.set_addr_domain([0, mem_params["capacity"] - 1])

    for s_ in port_types:
        s = s_ + "_info"
        mem_params[s] = [p.port_info for p in mem_params[s[:-4] + "ports"]]
        mem_params["num_" + s_ + "_ports"] = len(mem_params[s])
        # ports are not JSON serializable, so just store
        # port info
        del mem_params[s_ + "_ports"]

    return mem_params


def mem_inst(mem_params, mem_collateral={}):
    # print(mem_params)

    mem_params = port_to_info(mem_params)
    mem = Memory(mem_params)
    get_params(mem, mem_collateral, "mem")

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

        super().__init__("lake_mem", debug=True)

        # print("MEM PARAMS ", mem_params)

        self.mem_name = mem_params["name"]
        self.capacity = mem_params["capacity"]

        self.word_width = mem_params["word_width"]

        if "num_read_ports" not in mem_params:
            self.num_read_ports = 0
        else:
            self.num_read_ports = mem_params["num_read_ports"]

        if "num_write_ports" not in mem_params:
            self.num_write_ports = 0
        else:
            self.num_write_ports = mem_params["num_write_ports"]

        if "num_read_write_ports" not in mem_params:
            self.num_read_write_ports = 0
        else:
            self.num_read_write_ports = mem_params["num_read_write_ports"]

        self.write_info = mem_params["write_info"]
        self.read_info = mem_params["read_info"]
        self.read_write_info = mem_params["read_write_info"]

        self.og_num_write_ports = mem_params["num_write_ports"]
        self.og_num_read_ports = mem_params["num_read_ports"]
        self.og_num_read_write_ports = mem_params["num_read_write_ports"]

        if self.num_read_write_ports == 0:
            self.write_width = mem_params["write_port_width"]  # max(write_port_width, read_write_port_width)
            self.read_width = mem_params["read_port_width"]  # max(read_port_width, read_write_port_width)
        else:
            self.write_width = mem_params["read_write_port_width"]
            self.read_width = mem_params["read_write_port_width"]
            self.num_write_ports = mem_params["num_read_write_ports"]
            self.num_read_ports = mem_params["num_read_write_ports"]

        assert self.capacity % self.write_width == 0
        assert self.capacity % self.read_width == 0

        self.write_width_bits = max(1, clog2(self.write_width))
        self.read_width_bits = max(1, clog2(self.read_width))
        self.write_bits = clog2((self.capacity / self.write_width))
        self.read_bits = clog2((self.capacity / self.read_width))
        self.addr_width = max(1, clog2(self.capacity))

        # inputs
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)

        self.data_in = self.input("data_in",
                                  width=self.word_width,
                                  size=(self.num_write_ports, self.write_width),
                                  explicit_array=True)
        #                     packed=True)

        self.data_out = self.output("data_out",
                                    width=self.word_width,
                                    size=(self.num_read_ports, self.read_width),
                                    explicit_array=True)
        #                       packed=True)

        self.memory = self.var("memory",
                               width=self.word_width,
                               size=self.capacity,
                               explicit_array=True,
                               packed=True)

        # clean up
        if self.og_num_write_ports != 0 and self.og_num_read_ports != 0:
            self.write_addr = self.input("write_addr",
                                         width=self.addr_width,
                                         size=self.num_write_ports,
                                         explicit_array=True)
            #                         packed=True)
            # assert self.write_info["latency"] == 1, \
            #         "Write port latency 0 not supported."
            self.write = self.input("write",
                                    width=1,
                                    size=self.num_write_ports,
                                    explicit_array=True)
            #                         packed=True)
            self.add_code(self.write_data_latency_1)

            self.read_addr = self.input("read_addr",
                                        width=self.addr_width,
                                        size=self.num_read_ports,
                                        explicit_array=True)
            #                       packed=True)
            # for now assuming all read ports have same latency
            # also should add support for other latencies
            if self.read_info[0]["latency"] == 1:
                self.add_code(self.read_data_latency_1)
            else:
                self.add_code(self.read_data_latency_0)

        elif self.og_num_read_write_ports != 0:

            # clean up
            self.read_write_addr = self.input("read_write_addr",
                                              width=self.addr_width,
                                              size=self.num_read_write_ports,
                                              explicit_array=True)
            #                             packed=True)
            self.write_addr = self.var("write_addr",
                                       width=self.addr_width,
                                       size=self.num_read_write_ports,
                                       explicit_array=True)
            #                      packed=True)
            self.read_addr = self.var("read_addr",
                                      width=self.addr_width,
                                      size=self.num_read_write_ports,
                                      explicit_array=True)
            #                    packed=True)

            self.wire(self.write_addr, self.read_write_addr)
            self.wire(self.read_addr, self.read_write_addr)

            self.write = self.input("write",
                                    width=1,
                                    size=self.num_write_ports,
                                    explicit_array=True)
            #                         packed=True)

            self.add_code(self.read_data_latency_1)
            self.add_code(self.write_data_latency_1)

    # @always_ff((posedge, "clk"), (negedge, "rst_n"))
    @always_ff((posedge, "clk"))
    def write_data_latency_1(self):
        # if ~self.rst_n:
        #     for i in range(self.capacity):
        #         self.memory[i] = 0
        # else:
        for p in range(self.num_write_ports):
            if self.write[p]:
                for i in range(self.write_width):
                    self.memory[self.write_addr[p] + i] = self.data_in[p][i]

    @always_comb
    def read_data_latency_0(self):
        for p in range(self.num_read_ports):
            for port in range(self.read_width):
                self.data_out[p][port] = self.memory[self.read_addr[p] + port]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def read_data_latency_1(self):
        for p in range(self.num_read_ports):
            for port in range(self.read_width):
                self.data_out[p][port] = self.memory[self.read_addr[p] + port]


if __name__ == "__main__":

    agg_write_port = MemPort(1, 0)
    agg_read_port = MemPort(0, 0)

    agg_params = {"name": "agg",
                  "capacity": 4,
                  "word_width": 16,
                  "num_read_ports": 1,
                  "read_port_width": 4,
                  "num_write_ports": 1,
                  "write_port_width": 1,
                  "chaining": 0,
                  "write_ports": [agg_write_port],
                  "read_ports": [agg_read_port]}

    colat = {}
    agg = mem_inst(agg_params, colat)
    verilog(agg, filename="agg_mem.sv")

    sram_read_write_port = MemPort(1, 0)

    sram_params = {"name": "sram",
                   "capacity": 512,
                   "word_width": 16,
                   "num_read_write_ports": 1,
                   "read_write_port_width": 4,
                   "chaining": 1,
                   "read_write_ports": [sram_read_write_port]}

    colat = {}
    sram = mem_inst(sram_params, colat)
    verilog(sram, filename="sram_mem.sv")
