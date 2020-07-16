import magma as m


def create_circuit():
    @m.circuit.sequential(async_reset=True)
    class DelayBy2:
        def __init__(self):
            self.x: m.Bits[2] = m.bits(0, 2)
            self.y: m.Bits[2] = m.bits(0, 2)

        def __call__(self, I: m.Bits[2]) -> m.Bits[2]:
            O = self.y
            self.y = self.x
            self.x = I
            return O

    return DelayBy2


create_circuit()
