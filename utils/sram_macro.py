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
