from kratos import *
from lake.modules.for_loop import ForLoop
from lake.utils.util import safe_wire
from lake.modules.addr_gen import AddrGen


class Edge(Generator):
    def __init__(self,
                 from_signal,
                 to_signal,
                 addr_gen_dim,
                 addr_gen_max_range,
                 addr_gen_max_stride):

        super().__init__(f"edge_{from_signal}_{to_signal}", debug=True)

        # data_out
        self.from_signal = from_signal
        # data_in
        self.to_signal = to_signal

        #self.edges.append
        forloop = ForLoop(iterator_support=addr_gen_dim,
                          config_width=clog2(addr_gen_max_range))

        self._write(f"write_{to_signal}", 
                    width=1)

        # get memory params from top Lake or make a wrapper func for user
        # with just these params and then pass in mem for this signal
        # self._write_addr(f"write_addr_{to_signal}")

        self.add_child(f"loops_{from_signal}_{to_signal}",
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


        #self.add_child
