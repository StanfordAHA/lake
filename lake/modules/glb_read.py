import kratos


class GLBRead(kratos.Generator):

    def __init__(self, ID=0):
        # super().__init__(name=f"glb_read_{ID}")
        super().__init__(name=f"glb_read")

        self.external = True
        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._data = self.input("data", 16)

        self._ready = self.output("ready", 1)

        self._valid = self.input("valid", 1)

        self._done = self.output("done", 1)

        self._flush = self.input("flush", 1)

        self.NUM_BLOCKS_PARAM = self.param("NUM_BLOCKS", 32, 1)
