class SRAMMacroInfo:
    def __init__(self,
                 name="default_name",
                 addr_port_name="A",
                 ce_port_name="CEB",
                 clk_port_name="CLK",
                 data_in_port_name="D",
                 data_out_port_name="Q",
                 wen_port_name="WEB",
                 wtsel_port_name="WTSEL",
                 rtsel_port_name="RTSEL",
                 wtsel_value=0,
                 rtsel_value=0):

        self.name = name
        self.addr_port_name = addr_port_name
        self.ce_port_name = ce_port_name
        self.clk_port_name = clk_port_name
        self.data_in_port_name = data_in_port_name
        self.data_out_port_name = data_out_port_name
        self.wen_port_name = wen_port_name
        self.wtsel_port_name = wtsel_port_name
        self.rtsel_port_name = rtsel_port_name

        self.wtsel_value = wtsel_value
        self.rtsel_value = rtsel_value

        self.ports = (addr_port_name,
                      ce_port_name,
                      clk_port_name,
                      data_in_port_name,
                      data_out_port_name,
                      wen_port_name,
                      wtsel_port_name,
                      rtsel_port_name)

    def __hash__(self):
        return hash(self.ports)

    def __eq__(self, other: "SRAMMacroInfo"):
        return self.ports == other.ports
