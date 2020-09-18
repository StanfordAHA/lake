class MemPort():
    def __init__(self,
                 latency,
                 consecutive_latency):

        self.port_info = {"latency": latency, "consecutive_latency": consecutive_latency}

class Edge():
