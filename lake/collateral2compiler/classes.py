class MemPort():
    def __init__(self,
                 latency,
                 consecutive_latency):

        self.port_info = {"latency": latency, "consecutive_latency": consecutive_latency}

class Edge():
    def __init__(self,
                 from_signal,
                 to_signal,
                 addr_gen_dim,
                 addr_gen_max_range,
                 addr_gen_max_stride):

        self.from_signal = from_signal
        self.to_signal = to_signal
