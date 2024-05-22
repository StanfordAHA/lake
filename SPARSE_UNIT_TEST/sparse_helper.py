import enum
import os


class ControlCodeOnyx(enum.Enum):
    STOP = 0
    DONE = 1
    MAYBE = 2


def set_bit(old_val, bit_to_set, new_bit):
    new_val = old_val | (new_bit << bit_to_set)
    return new_val


def get_bit(val, n):
    return val >> n & 1


def is_DONE(s_):
    return s_ == 0x10100


def is_STOP(s_):
    return (s_ >> 16) == 1 and (not is_DONE(s_)) and (s_ != 0x10200)


def is_DONE_sam(s_):
    if type(s_) is int:
        return False
    if s_ == 'D':
        return True
    else:
        return False


def is_STOP_sam(s_, level = -1):
    if type(s_) is int:
        return False 
    if s_[0] == 'S':
        if level == -1:
            return True
        else:
            return level == int(s_[1:])
    else:
        return False


def is_MAYBE_sam(s_):
    if type(s_) is int:
        return False
    if s_ == 'N':
        return True
    else:
        return False


def convert_stream_to_onyx_interp(stream):

    ctrl_op_offset = 8
    num_ctrl_bits = 2
    top_bit = 17 - 1

    converted_stream = []
    for s_ in stream:
        if type(s_) is int:
            converted_stream.append(s_)
        elif type(s_) is str and s_ == 'R':
            converted_stream.append(1)
        elif type(s_) is str:
            control_code = 0
            if 'S' in s_:
                if s_ != 'S':
                    control_code = int(s_.lstrip('S'))
            elif 'D' in s_:
                set_ctrl = ControlCodeOnyx.DONE.value
                for offset_ in range(num_ctrl_bits):
                    bts = get_bit(set_ctrl, offset_)
                    control_code = set_bit(control_code, ctrl_op_offset + offset_, bts)
            elif 'N' in s_:
                set_ctrl = ControlCodeOnyx.MAYBE.value
                for offset_ in range(num_ctrl_bits):
                    bts = get_bit(set_ctrl, offset_)
                    control_code = set_bit(control_code, ctrl_op_offset + offset_, bts)
            else:
                raise NotImplementedError
            control_code = set_bit(control_code, top_bit, 1)
            converted_stream.append(control_code)
        else:
            raise NotImplementedError
    assert len(converted_stream) == len(stream), \
        f"Input length {len(stream)} didn't match output length {len(converted_stream)}"
    return converted_stream


def read_txt(file_name, count=1):
    r = []
    with open(file_name, "r") as f:
        for line in f:
            r.append(int(line, 16))
            if int(line, 16) == 0x10100:
                count -= 1
            if count == 0:
                break
        f.close()
    return r


def read_glb(file_name, tx_num=1):
    r = []
    has_length = 0
    total_count = tx_num
    length_count = 0
    stream_count = 0
    with open(file_name, "r") as f:
        r_sub = []
        for line in f:
            v = int(line, 16)
            if v > 0x7FFF:  # assuming everything is signed 16-bit
                v -= 0x10000
            if has_length == 0:
                r_sub = []
                length_count = v
                r_sub.append(v)
                has_length = 1
                # print("get length")
                # print(r)
            else:
                length_count -= 1
                r_sub.append(v)
                
            if length_count == 0:
                total_count -= 1
                has_length = 0
                r.append(r_sub)
            if total_count == 0:
                break
        f.close()
    return r


def tohex(val, nbits):
  return hex((val + (1 << nbits)) % (1 << nbits))

def write_txt(file_name, data):
    with open(file_name, "w") as f:
        for element in data:
            # handle negative data values
            if element < 0: 
                hex_element = tohex(element, 16)
                f.write(f'{hex_element[2:]}' + '\n')
            else:
                f.write(f'{element:x}' + '\n')
            
        f.close()
    return


def clear_txt(file_name):
    open(file_name, "w").close()
    return


def update_tcl(tb):
    template = "unit_test_template.tcl"
    new_tcl = "unit_test_fsdb.tcl"
    with open(template, "r") as f:
        lines = f.readlines()
        f.close()
    #replace xxx with tb
    with open(new_tcl, "w") as f:
        for line in lines:
            if "xxx" in line:
                line = line.replace("xxx", tb)
            f.write(line)
        f.close()
    return 


def coord_drop(coord):
    cleaned = []
    pre_s = True
    for i in coord:
        if is_STOP_sam(i):
            if not pre_s:
                cleaned.append(i)
                pre_s = True
        else:
            pre_s = False
            cleaned.append(i)
    return cleaned
