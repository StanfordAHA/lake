import kratos


class GLBWrite(kratos.Generator):

    def __init__(self, ID=0):
        # super().__init__(name=f"glb_write_{ID}")
        super().__init__(name=f"glb_write")

        self.external = True
        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._data = self.output("data", 16)

        self._ready = self.input("ready", 1)

        self._valid = self.output("valid", 1)

        self._done = self.output("done", 1)

        self._flush = self.input("flush", 1)

        self.TX_SIZE_PARAM = self.param("TX_SIZE", 32, 32)

        self.FILE_NO_PARAM = self.param("FILE_NO", 32, 0)
