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
        # port info instead of port obejcts
        del mem_params[s_ + "_ports"]

    return mem_params


def mem_inst(mem_params, word_width, mem_collateral={}):
    # print(mem_params)

    # get full memory parameters with port information
    mem_params = port_to_info(mem_params)
    # create memory instance
    mem = Memory(mem_params, word_width)
    # get memory parameters for compiler collateral if needed
    get_params(mem, mem_collateral, "mem")

    return mem


class Memory(Generator):
    def __init__(self,
                 mem_params,
                 word_width):

        super().__init__("lake_mem", debug=True)

        # print("MEM PARAMS ", mem_params)

        self.word_width = word_width

        self.mem_name = mem_params["name"]
        self.capacity = mem_params["capacity"]

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

        # either read_width or write_width will be 1, because
        # we can always reduce down to this case
        # if we have read_write_ports, we assume read_width = write_width

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
        self.write_bits = clog2(int(self.capacity / self.write_width))
        print("PARAMS: ", self.capacity, " ", self.read_width, " ", self.write_bits)
        self.read_bits = clog2(int(self.capacity / self.read_width))
        self.addr_width = max(1, clog2(self.capacity))

        # inputs
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)

        self.data_in = self.input("data_in",
                                  width=self.word_width,
                                  size=(self.num_write_ports, self.write_width),
                                  explicit_array=True,
                                  packed=True)

        self.data_out = self.output("data_out",
                                    width=self.word_width,
                                    size=(self.num_read_ports, self.read_width),
                                    explicit_array=True,
                                    packed=True)

        self.mem_size = max(self.read_width, self.write_width)

        assert self.capacity % self.mem_size == 0
        # assert self.read_width == 1 or self.write_width == 1 or self.read_width == self.write_width
        self.mem_last_dim = int(self.capacity / self.mem_size)

        self.memory = self.var("memory",
                               width=self.word_width,
                               # size=self.capacity,
                               size=(self.mem_last_dim, self.mem_size),
                               explicit_array=True,
                               packed=True)

        # clean up
        if self.og_num_write_ports != 0 and self.og_num_read_ports != 0:
            self.write_addr = self.input("write_addr",
                                         width=16,  # self.addr_width,
                                         size=self.num_write_ports,
                                         explicit_array=True)
            #                        packed=True)

            self.write = self.input("write",
                                    width=1,
                                    size=self.num_write_ports)
            #                         explicit_array=True)
            #                         packed=True

            assert self.write_info[0]["latency"] > 0
            self.add_write_data_block()
            # self.add_code(self.write_data_latency_1)

            self.read_addr = self.input("read_addr",
                                        width=16,  # self.addr_width,
                                        size=self.num_read_ports,
                                        explicit_array=True)
            #                       packed=True)

            # for now assuming all read ports have same latency
            # also should add support for other latencies
            # if self.read_info[0]["latency"] == 1:
            #    self.add_code(self.read_data_latency_1)
            # else:
            #    self.add_code(self.read_data_latency_0)
            self.add_read_data_block()

        elif self.og_num_read_write_ports != 0:

            # clean up
            self.read_write_addr = self.input("read_write_addr",
                                              width=16,  # self.addr_width,
                                              size=self.num_read_write_ports,
                                              explicit_array=True)
            #                             packed=True)
            self.write_addr = self.var("write_addr",
                                       width=16,  # self.addr_width,
                                       size=self.num_read_write_ports,
                                       explicit_array=True)
            #                      packed=True)
            self.read_addr = self.var("read_addr",
                                      width=16,  # self.addr_width,
                                      size=self.num_read_write_ports,
                                      explicit_array=True)
            #                    packed=True)

            self.wire(self.write_addr, self.read_write_addr)
            self.wire(self.read_addr, self.read_write_addr)

            self.write = self.input("write",
                                    width=1,
                                    size=self.num_write_ports)
            #                         explicit_array=True)
            #                         packed=True)

            # TO DO
            self.read_info = []
            self.read_info.append({"latency": 1})
            self.add_read_data_block()
            # self.add_code(self.read_data_latency_1)
            self.add_write_data_block()
#            self.add_code(self.write_data_latency_1)

    def add_write_data_block(self):
        print("mem last dim ", self.mem_last_dim, " ", self.write_width)
        if self.write_width > 1:
            print("write 1 0")
            self.add_code(self.write_data_latency_1_0)
        elif self.mem_last_dim == 1:
            print("write 1 1")
            self.add_code(self.write_data_latency_1_1)
        else:
            print("write 1 2")
            self.add_code(self.write_data_latency_1_2)

    def add_read_data_block(self):
        print("mem last dim", self.mem_last_dim)
        print("read width", self.read_width, " ", self.read_bits)
        if self.read_info[0]["latency"] == 1:
            if self.read_width > 1 and self.mem_last_dim > 1:
                print("read 1 0")
                self.add_code(self.read_data_latency_1_0)
            elif self.mem_last_dim == 1:
                print("read 1 1")
                self.add_code(self.read_data_latency_1_1)
            else:
                print("read 1 2")
                self.add_code(self.read_data_latency_1_2)
        else:
            if self.read_width > 1:
                if self.mem_last_dim > 1:
                    print("read 0 0")
                    self.add_code(self.read_data_latency_0_0)
                # self.mem_last_dim == 1
                else:
                    print("read 0 1")
                    self.add_code(self.read_data_latency_0_1)
            else:
                print("read 0 2")
                self.add_code(self.read_data_latency_0_2)

    # if self.write_width > 1:
    @always_ff((posedge, "clk"))
    def write_data_latency_1_0(self):
        for p in range(self.num_write_ports):
            if self.write[p]:
                self.memory[self.write_addr[p][clog2(self.mem_last_dim) - 1, 0]] = self.data_in[p]

    # write_width is 1 and equal to width of memory
    @always_ff((posedge, "clk"))
    def write_data_latency_1_1(self):
        for p in range(self.num_write_ports):
            if self.write[p]:
                self.memory[0][self.write_addr[p][self.write_width_bits - 1, 0]] = self.data_in[p]

    # write_width is 1 but less than the width of memory
    @always_ff((posedge, "clk"))
    def write_data_latency_1_2(self):
        for p in range(self.num_write_ports):
            if self.write[p]:
                self.memory[self.write_addr[p]
                            [clog2(self.mem_last_dim) + self.write_width_bits - 1, self.write_width_bits]] \
                    [self.write_addr[p][self.write_width_bits - 1, 0]] \


    # if self.read_width > 1:
    @always_comb
    def read_data_latency_0_0(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[self.read_addr[p][clog2(self.mem_last_dim) - 1, 0]]

    # read_width is 1 and equal to width of memory
    @always_comb
    def read_data_latency_0_1(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[0]  # [self.read_addr[p]
            # [self.read_width - 1, 0]]

    # read width is 1 but less than the width of memory
    @always_comb
    def read_data_latency_0_2(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[self.read_addr[p]
                                           [clog2(self.mem_last_dim) + self.read_width_bits - 1, self.read_width_bits]] \
                [self.read_addr[p][self.read_width_bits - 1, 0]]

    # if self.read_width > 1:
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def read_data_latency_1_0(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[self.read_addr[p][clog2(self.mem_last_dim) - 1, 0]]

    # read_width is 1 and equal to width of memory
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def read_data_latency_1_1(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[0][self.read_addr[p]
                                              [self.read_bits - 1, 0]]

    # read width is 1 but less than the width of memory
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def read_data_latency_1_2(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[self.read_addr[p]
                                           [clog2(self.mem_last_dim) + self.read_bits - 1, self.read_bits]] \
                [self.read_addr[p][self.read_bits - 1, 0]]


if __name__ == "__main__":
    agg_write_port = MemPort(1, 0)
    agg_read_port = MemPort(0, 0)

    agg_params = {"name": "agg",
                  "capacity": 4,
                  "read_port_width": 4,
                  "write_port_width": 1,
                  "num_read_ports": 1,
                  "num_write_ports": 1,
                  "num_read_write_ports": 0,
                  "write_ports": [agg_write_port],
                  "read_ports": [agg_read_port],
                  "read_write_ports": []}

    print("agg")
    agg = mem_inst(agg_params, 16)
    verilog(agg, filename="agg_mem.sv")

    sram_read_write_port = MemPort(1, 0)

    sram_params = {"name": "sram",
                   "capacity": 512,
                   "read_write_port_width": 4,
                   "num_read_ports": 0,
                   "num_write_ports": 0,
                   "num_read_write_ports": 1,
                   "read_ports": [],
                   "write_port": [],
                   "read_write_ports": [sram_read_write_port]}

    print("sram")
    sram = mem_inst(sram_params, 16)
    verilog(sram, filename="sram_mem.sv")

    tb_write_port = MemPort(1, 0)
    tb_read_port = MemPort(0, 0)

    tb_params = {"name": "tb",
                 "capacity": 8,
                 "read_port_width": 1,
                 "write_port_width": 4,
                 "num_read_ports": 1,
                 "num_write_ports": 1,
                 "num_read_write_ports": 0,
                 "write_ports": [tb_write_port],
                 "read_ports": [tb_read_port],
                 "read_write_ports": []}

    print("tb")
    tb = mem_inst(tb_params, 16)
    verilog(tb, filename="tb_mem.sv")
