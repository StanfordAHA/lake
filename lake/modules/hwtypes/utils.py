def get_slice(data, addr_, width):
    addr = addr_.zext(data.size - addr_.size)
    print("mem slice ", data)
    print("data >> addr ", data >> addr)
    print("width ", width)
    print("data >> addr w", (data >> addr)[:width])
    return (data >> addr)[:width]

def set_slice(data, addr_, width, value):
    assert value.size == width
    #print("set_slice func")
    #print("data ", data)
    #print("addr ", addr_)
    #print("width ", width)
    #print("value ", value)
    addr = addr_.zext(data.size - addr_.size)
    print("addr ", addr)
    top_bits = (data >> (addr + width)) << (addr + width)
    mid_bits = value.zext(data.size - width) << addr
    print("mid bits ", mid_bits >> addr)
    bot_bits = (data << (addr + width)) >> (addr + width)
    #print(top_bits | mid_bits | bot_bits)
    return top_bits | mid_bits | bot_bits

