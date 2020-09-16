class MemPort():
    def __init__(self,
                 latency,
                 consecutive_latency,
                 gen_dim=6,
                 gen_range=65536,
                 gen_stride=65536):

        self.port_info = {"latency": latency, "consecutive_latency": consecutive_latency}
