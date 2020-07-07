def get_slice(data, addr_, width):
    addr_ *= width
    if type(addr_) != int:
        addr = addr_.zext(data.size - addr_.size)
    else:
        addr = addr_
    #print("mem slice ", data)
    #print("data >> addr ", data >> addr)
    #print("width ", width)
    #print("data >> addr w", (data >> addr)[:width])
    print((data >> addr)[:width])
    print("mem: ", data)
    return (data >> addr)[:width]

def set_slice(data, addr_, width, value):
    assert value.size == width
    print("addr_ in ", addr_)
    print("new addr_ : ", addr_)
    #print("set_slice func")
    #print("data ", data)
    print("before addr ", addr_)
    #print("width ", width)
    #print("value ", value)
    print("addr size ", data.size, " ", addr_.size, " ", data.size - addr_.size)
    addr = addr_.zext(data.size - addr_.size)
    print("addr_ ", addr_)
    print(addr_.size)
    print("after addr ", addr)
    print("after addr size ", addr.size)
    top_bits = (data >> (addr + width)) << (addr + width)
    mid_bits = value.zext(data.size - width) << addr
    print("mid bits ", mid_bits >> addr)
    bot_bits = (data << (addr + width)) >> (addr + width)
    print("top ", top_bits, " mid ", mid_bits, " bot ", bot_bits)
    #print(top_bits | mid_bits | bot_bits)
    print("get slices")
    print("0 ", get_slice(data, 0, 16))
    print("1 ", get_slice(data, 1, 16))
    print("2 ", get_slice(data, 2, 16))
    print("3 ", get_slice(data, 3, 16))
    return top_bits | mid_bits | bot_bits

