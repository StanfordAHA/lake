from kratos import *
from lake.modules.for_loop import ForLoop
from lake.utils.util import safe_wire
from lake.modules.addr_gen import AddrGen
from lake.dsl.helper import *


def get_full_edge_params(edge_params):
    if "dim" not in edge_params:
        edge_params["dim"] = 6
    if "max_range" not in edge_params:
        edge_params["max_range"] = 65535
    if "max_stride" not in edge_params:
        edge_params["max_stride"] = 65535


def edge_inst(edge_params, from_mem, to_mem, from_inst, to_inst, edge_collateral):

    edge = Edge(edge_params, from_mem, to_mem, from_inst, to_inst)
    get_params(edge, edge_collateral, "edge")

    return edge


class Edge(Generator):
    def __init__(self,
                 edge_params,
                 from_mem,
                 to_mem,
                 from_inst,
                 to_inst):

        super().__init__(f"lake_edge", debug=True)

        # PARAMETERS
        # data_out
        self.from_signal = edge_params["from_signal"]
        # data_in
        self.to_signal = edge_params["to_signal"]
        self.dim = edge_params["dim"]
        self.max_range = edge_params["max_range"]
        self.max_stride = edge_params["max_stride"]

        self._write(f"write_{self.to_signal}",
                    width=1)

        forloop = ForLoop(iterator_support=self.dim,
                          config_width=clog2(self.max_range))

        # get memory params from top Lake or make a wrapper func for user
        # with just these params and then pass in mem for this signal
        # self._write_addr(f"write_addr_{to_signal}")

        self.add_child(f"loops_{self.from_signal}_{self.to_signal}",
                       forloop,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write)

        AG_write = AddrGen(iterator_support=addr_gen_dim,
                           config_width=clog2(addr_gen_max_range))

        self.add_child(f"AG_write_{self.from_signal}_{self.to_signal}",
                       AG_write,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write,
                       mux_sel=forloop.ports.mux_sel_out)

        safe_wire(self, AG_write.ports.addr_out, self._write_addr)
