import kratos as kts


def increment(var, value):
    return var + kts.const(value, var.width)


def decrement(var, value):
    return var - kts.const(value, var.width)


def transpose(generator: kts.Generator, port, name)
    tpose_var = generator.var(name,
                              port.width,
                              port.size,
                              explicit_array=True,
                              packed=True)