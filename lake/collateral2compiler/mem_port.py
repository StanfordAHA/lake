class MemPort():
    def __init__(self,
                 latency,
                 II):

        self.port_info = {"latency": latency, "initiation_interval": II}


class Edge():
    def __init__(self,
                 from_signal,
                 to_signal,
                 addr_gen_dim,
                 addr_gen_max_range,
                 addr_gen_max_stride):

        self.from_signal = from_signal
        self.to_signal = to_signal
