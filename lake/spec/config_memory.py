import kratos
import random as rand
from lake.attributes.config_reg_attr import ConfigRegAttr
from kratos import PortDirection, always_ff, posedge, negedge
from lake.modules.ready_valid_interface import RVInterface
from lake.utils.spec_enum import Direction

class ConfigMemory(kratos.Generator):

    """
        Config memory wrapper module
    """
    def __init__(self, width, name=""):
        if name == "":
            super().__init__(f"config_memory_{width}")
        else:
            super().__init__(f"config_memory_{width}_{name}")

        self.width = width

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        # self._flush = self.reset("flush", is_async=False)

        self._flush = self.input("flush", 1)
        # self.add_attribute("sync-reset=flush")

        self._config_memory = self.input("config_memory", self.width, packed=True)
        # Wrap it in a module
        self._config_memory_harden = self.var("config_memory_harden", self.width, packed=True)
        self._config_memory_harden_en = self.input("config_memory_wen", 1)

        self._config_memory_out = self.output("config_memory_out", self.width, packed=True)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def hardened_config_flop():
            if ~self._rst_n:
                self._config_memory_harden = 0
            elif self._config_memory_harden_en:
                self._config_memory_harden = self._config_memory
        self.add_code(hardened_config_flop)

        self.wire(self._config_memory_out, self._config_memory_harden)

    def get_width(self):
        return self.width
