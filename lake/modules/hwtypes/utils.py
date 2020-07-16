def get_slice(data, addr, width):
    addr = addr.zext(data.size - addr.size)
    addr = addr * width
    return (data >> addr)[:width]


def set_slice(data, addr, width, value):
    assert value.size == width
    addr = addr.zext(data.size - addr.size)
    addr = addr * width
    value = value.zext(data.size - value.size)
    # Shift out bottom bits / shift in zeros
    top_bits = (data >> (addr + width)) << (addr + width)
    # shift to address
    mid_bits = value << addr
    # shift out top bits / shift in zeros
    bot_bits = (data << (-addr + data.size)) >> (-addr + data.size)
    return top_bits | mid_bits | bot_bits
