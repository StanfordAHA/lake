from kratos import *
from lake.modules.for_loop import ForLoop
from lake.utils.util import safe_wire
from lake.modules.addr_gen import AddrGen
from lake.collateral2compiler.helper import *


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
        self.dim = edge_params["dim"] if "dim" in edge_params else 6
        self.max_range = edge_params["max_range"] if "max_range" in edge_params else 65536
        self.max_stride = edge_params["max_stride"] if "max_stride" in edge_params else 65536

        # self.edges.append
        forloop = ForLoop(iterator_support=self.dim,
                          config_width=clog2(self.max_range))

        self._write(f"write_{self.to_signal}",
                    width=1)

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

        self.add_child(f"AG_write_{from_signal}_{to_signal}",
                       AG_write,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write,
                       mux_sel=forloop.ports.mux_sel_out)

        safe_wire(self, AG_write.ports.addr_out, self._write_addr)

        # self.add_child
