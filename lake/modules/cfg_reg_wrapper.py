from kratos import *
import kratos as kts
from _kratos import create_wrapper_flatten
from math import log

from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.dsl.edge import get_full_edge_params
from lake.dsl.helper import *
from lake.dsl.mem_port import MemPort
from lake.dsl.memory import mem_inst
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.passes.passes import lift_config_reg
from lake.utils.util import safe_wire, trim_config_list
from lake.utils.util import extract_formal_annotation, modular_formal_annotation
from lake.utils.parse_clkwork_config import *


'''
    Basically creating configuration wrapper with
    bogus config bus for synthesis numbers.
'''


class CFGRegWrapper(Generator):
    def __init__(self,
                 child_gen: kts.Generator):

        super().__init__(f"{child_gen.name}_cfg_W", debug=True)

        # Works for now since we always have these...
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self.add_child_generator(f"{child_gen.name}", child_gen,
                                 clk=self._clk,
                                 rst_n=self._rst_n)

        # Create fake config bus...
        self._config_en = self.input("config_en", 1)
        self._config_data = self.input("config_data", 256)

        # Now copy inputs and outputs that aren't config regs

        for port in child_gen.ports:
            actual_port = child_gen.ports[port]
            self.uplevel_port(actual_port)

    def uplevel_port(self, port):

        pname = port.name
        pdir = str(port.port_direction)
        ptype = str(port.port_type)
        pattrs = port.get_attributes()

        if pname == "clk" or pname == "rst_n":
            return

        if ptype == "PortType.Data" and pdir == "PortDirection.In":
            cfgreg_attr = False
            for attr in pattrs:
                if isinstance(attr, ConfigRegAttr):
                    cfgreg_attr = True
            # We know to hook it up to bogus config bus
            if cfgreg_attr is True:
                newvar = self.var_from_def(port, pname)
                self.wire(newvar, port)

                @always_ff((posedge, "clk"), (negedge, "rst_n"))
                def add_to_config_reg(self):
                    if ~self._rst_n:
                        newvar = 0
                    elif self._config_en:
                        newvar = 1

                self.add_code(add_to_config_reg)
            # Otherwise pass it through...
            else:
                newin = self.port_from_def(port, pname)
                self.wire(newin, port)
        else:
            newport = self.port_from_def(port, pname)
            self.wire(newport, port)
