from lake.collateral2compiler.memory import mem_inst


class Lake():
    def __init__(self):
        self.mem_collateral = {}

    # default for ports is no ports
    def add_memory(mem_params, write_ports=[], read_ports=[], read_write_ports=[]):

        mem_params["num_write_ports"] = len(write_ports)
        mem_params["num_read_ports"] = len(read_ports)
        # mem_params["num_read_write_ports"] = len(read_write_ports)

        mem_params["write_info"] = [port.port_info for port in write_ports]
        mem_params["read_info"] = [port.port_info for port in read_ports]
        # mem_params["read_write_info"] = [port.port_info for port in read_write_ports]

        mem = mem_inst(mem_params, self.mem_collateral)
