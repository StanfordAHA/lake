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

        ################################################################
        # PARAMETERS
        ################################################################
        # print("MEM PARAMS ", mem_params)

        # basic parameters
        self.word_width = word_width

        # general memory parameters
        self.mem_name = mem_params["name"]
        self.capacity = mem_params["capacity"]

        # number of port types
        self.num_read_write_ports = mem_params["num_read_write_ports"]
        self.num_read_only_ports = mem_params["num_read_ports"]
        self.num_write_only_ports = mem_params["num_write_ports"]
        self.num_read_ports = self.num_read_only_ports + self.num_read_write_ports
        self.num_write_ports = self.num_write_only_ports + self.num_read_write_ports

        # info for port types
        self.write_info = mem_params["write_info"]
        self.read_info = mem_params["read_info"]
        self.read_write_info = mem_params["read_write_info"]

        # TO DO change - for now, we assume you cannot have read/write and read or write ports
        # should be the max of write vs read_write and need to handle more general case
        if self.num_read_write_ports == 0:
            self.write_width = mem_params["write_port_width"]
            self.read_width = mem_params["read_port_width"]
        else:
            self.write_width = mem_params["read_write_port_width"]
            self.read_width = mem_params["read_write_port_width"]

        assert self.capacity % self.write_width == 0, \
            "Memory capacity is not a multiple of the port width for writes"
        assert self.capacity % self.read_width == 0, \
            "Memory capacity is not a multiple of the port width for reads"

        # innermost dimension for size of memory is the size of whichever port
        # type has a wider width between reads and writes
        self.mem_size = max(self.read_width, self.write_width)
        # this assert has to be true if previous two asserts are true
        assert self.capacity % self.mem_size == 0
        # this is the last dimension for size of memory - equal to the number
        # of the port type with wider width addresses can fit in the memory
        self.mem_last_dim = int(self.capacity / self.mem_size)

        self.mem_size_bits = max(1, clog2(self.mem_size))
        self.mem_last_dim_bits = max(1, clog2(self.mem_last_dim))

        # minimum required widths for address signals
        if self.mem_size == self.write_width:
            self.write_addr_width = self.mem_last_dim_bits
            self.read_addr_width = self.mem_size_bits + self.mem_last_dim_bits
        else:
            self.write_addr_width = self.mem_size_bits + self.mem_last_dim_bits
            self.read_addr_width = self.mem_last_dim_bits

        ################################################################
        # I/O INTERFACE (WITHOUT ADDRESSING) + MEMORY
        ################################################################
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)

        self.data_in = self.input("data_in",
                                  width=self.word_width,
                                  size=(self.num_write_ports, self.write_width),
                                  explicit_array=True,
                                  packed=True)

        # write enable (high: write, low: read)
        self.write = self.input("write",
                                width=1,
                                size=self.num_write_ports)

        self.data_out = self.output("data_out",
                                    width=self.word_width,
                                    size=(self.num_read_ports, self.read_width),
                                    explicit_array=True,
                                    packed=True)

        # memory variable (not I/O)
        self.memory = self.var("memory",
                               width=self.word_width,
                               size=(self.mem_last_dim, self.mem_size),
                               explicit_array=True,
                               packed=True)

        ################################################################
        # ADDRESSING I/O AND SIGNALS
        ################################################################

        # I/O is different depending on whether we have read and write ports or
        # read/write ports

        # we keep address width at 16 to avoid unpacked
        # safe_wire errors for addr in hw_top_lake - can change by changing
        # default_config_width for those addr gens while accounting for muxing
        # bits, but the extra bits are unused anyway

        # TO DO change later - same read/write or read and write assumption as above
        if self.num_write_only_ports != 0 and self.num_read_only_ports != 0:
            # writes
            self.write_addr = self.input("write_addr",
                                         width=16, # self.write_addr_width,
                                         size=self.num_write_ports,
                                         explicit_array=True)

            assert self.write_info[0]["latency"] > 0, \
                "Latency for write ports must be greater than 1 clock cycle."
            self.add_write_data_block()

            # reads
            self.read_addr = self.input("read_addr",
                                        width=16, # self.read_addr_width,
                                        size=self.num_read_ports,
                                        explicit_array=True)

            # TO DO for now assuming all read ports have same latency
            # TO DO also should add support for other latencies
            self.add_read_data_block()

        elif self.num_read_write_ports != 0:
            self.read_write_addr = self.input("read_write_addr",
                                              width=16, # max(self.read_addr_width, self.write_addr_width),
                                              size=self.num_read_write_ports,
                                              explicit_array=True)

            # writes
            self.write_addr = self.var("write_addr",
                                       width=16, # self.write_addr_width,
                                       size=self.num_read_write_ports,
                                       explicit_array=True)

            for p in range(self.num_read_write_ports):
                safe_wire(gen=self, w_to=self.write_addr[p], w_from=self.read_write_addr[p])

            self.add_write_data_block()

            # reads
            self.read_addr = self.var("read_addr",
                                      width=self.read_addr_width,
                                      size=self.num_read_write_ports,
                                      explicit_array=True)
            for p in range(self.num_read_write_ports):
                safe_wire(gen=self, w_to=self.read_addr[p], w_from=self.read_write_addr[p])

            # TO DO in self.read_write_info we should allow for different read
            # and write latencies?
            self.read_info = self.read_write_info
            self.add_read_data_block()

    def add_write_data_block(self):
        if self.write_width == self.mem_size:
            self.add_code(self.write_data_latency_1_0)
        elif self.write_width < self.mem_size:
            self.add_code(self.write_data_latency_1_1)
        else:
            print("Error: Write width > mem size which is not possible...please check code.")

    def add_read_data_block(self):
        if self.read_width == self.mem_size:
            if self.read_info[0]["latency"] == 1:
                self.add_code(self.read_data_latency_1_0)
            else:
                self.add_code(self.read_data_latency_0_0)
        elif self.read_width < self.mem_size:
            if self.read_info[0]["latency"] == 1:
                self.add_code(self.read_data_latency_1_1)
            else:
                self.add_code(self.read_data_latency_0_1)
        else:
            print("Error: Read width > mem size which is not possible...please check code.")

    # write width is same as innermost dimension for memory, so
    # just need to index with write addr in the last dimension of memory (mem_last_dim)
    @always_ff((posedge, "clk"))
    def write_data_latency_1_0(self):
        for p in range(self.num_write_ports):
            if self.write[p]:
                self.memory[self.write_addr[p][self.mem_last_dim_bits - 1, 0]] = self.data_in[p]

    # write width is less than innermost dimension of memory, so
    # last mem_size_bits will give innermost dimension for memory (mem_size) and
    # the next self.mem_last_dim_bits will give the last dimension of memory (mem_last_dim)
    @always_ff((posedge, "clk"))
    def write_data_latency_1_1(self):
        for p in range(self.num_write_ports):
            if self.write[p]:
                self.memory[self.write_addr[p][self.mem_last_dim_bits - 1 + self.mem_size_bits, self.mem_size_bits]] \
                    [self.write_addr[p][self.mem_size_bits - 1, 0]] = self.data_in[p]

    @always_comb
    def read_data_latency_0_0(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[self.read_addr[p][self.mem_last_dim_bits - 1, 0]]

    @always_comb
    def read_data_latency_0_1(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[self.read_addr[p][self.mem_last_dim_bits - 1 + self.mem_size_bits, self.mem_size_bits]] \
                [self.read_addr[p][self.mem_size_bits - 1, 0]]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def read_data_latency_1_0(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[self.read_addr[p][self.mem_last_dim_bits - 1, 0]]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def read_data_latency_1_1(self):
        for p in range(self.num_read_ports):
            self.data_out[p] = self.memory[self.read_addr[p][self.mem_last_dim_bits - 1 + self.mem_size_bits, self.mem_size_bits]] \
                [self.read_addr[p][self.mem_size_bits - 1, 0]]


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
