class SRAMMacroInfo:
    def __init__(self, 
                 name="default_name",
                 addr_port_name="ADDR",
                 ce_port_name="CEB",
                 clk_port_name="CLK",
                 data_in_port_name="D",
                 data_out_port_name="Q",
                 wen_port_name="WE"):

        self.name = name
        self.addr_port_name = addr_port_name
        self.ce_port_name = ce_port_name
        self.clk_port_name = clk_port_name
        self.data_in_port_name = data_in_port_name
        self.data_out_port_name = data_out_port_name
        self.wen_port_name = wen_port_name

        self.ports = (addr_port_name, 
                      ce_port_name, 
                      clk_port_name, 
                      data_in_port_name, 
                      data_out_port_name, 
                      wen_port_name)

    def get_name(self):
        return self.name

    def get_ports(self):
        return self.ports

    def set_name(self, name):
        self.name = name

    def set_addr_port_name(self, addr_port):
        self.addr_port_name = addr_port
        self.ports[0] = addr_port

    def set_ce_port_name(self, ce_port):
        self.ce_port_name = ce_port
        self.ports[1] = ce_port

    def set_clk_port_name(self, clk_port):
        self.clk_port_name = clk_port
        self.ports[2] = clk_port

    def set_data_in_port_name(self, data_in_port):
        self.data_in_port_name = self.data_in_port
        self.ports[3] = data_in_port

    def set_data_out_port_name(self, data_out_port):
        self.data_out_port_name = self.data_out_port
        self.ports[4] = data_out_port

    def set_wen_port_name(self, wen_port):
        self.wen_port_name = self.wen_port
        self.ports[5] = wen_port
