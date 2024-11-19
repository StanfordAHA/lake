from kratos import clog2
from math import ceil
import argparse


class MainMemoryModel():

    def __init__(self, fetch_width, capacity=4096, verbose=False) -> None:
        self.verbose = verbose
        self.fw = fetch_width
        self.fw_log_2 = clog2(fetch_width)
        self.capacity = capacity
        self.data_array = [0 for x in range(capacity)]
        # This should be a list
        self.addr_seq = None

        self.wcb = []

        self.wcb_num_items = 0
        self.wcb_tags = []

        self.wcb_max_items = 4
        self.wcb_max_items_log2 = clog2(self.wcb_max_items)

        self.addr_q = []
        self.pop_q = []

        self.addr_q_max_size = 8

        self.already_read = False
        self.last_read_addr = None
        self.read_last_cycle = False
        self.pop_addr_q = False
        self.pop_wcb = False
        self.cycle = 0
        self.addr_seq_set = False
        self.data_on_sram_output = False

    def write(self, data: list, address: int):
        # Need to make sure that the write data is the proper fetch width
        assert len(data) == self.fw
        self.data_array[address * self.fw:address * self.fw + self.fw] = data

    def get_curr_addr(self):
        assert self.addr_seq_set
        if len(self.addr_seq) > 0:
            return self.addr_seq[0]
        else:
            return None

    def next_addr(self):
        self.addr_seq.pop(0)

    def get_outer_address(self, address):
        # This address looks up into the wcb tags...
        inner_bits = self.fw_log_2
        tag_bits = self.wcb_max_items_log2
        ret_value = 0
        for i in range(tag_bits):
            masked_bit = (address >> (i + inner_bits)) & 1
            ret_value = ret_value | (masked_bit << i)

        return ret_value

    def get_inner_address(self, address):
        lowest_bits = self.fw_log_2
        ret_value = 0
        for i in range(lowest_bits):
            masked_bit = (address >> i) & 1
            ret_value = ret_value | (masked_bit << i)

        return ret_value

    def read_wcb(self):
        # Flatten address
        # curr_addr = self.get_curr_addr()
        # Current address comes from the queue!
        curr_addr = self.addr_q[0]
        # Get outer index tag
        # outer_index_tag = (curr_addr >> self.fw_log_2) & 1
        outer_index_tag = self.get_outer_address(curr_addr)
        # Get inner index
        inner_index = self.get_inner_address(curr_addr)
        if self.verbose:
            print(f"Current addr: {curr_addr}")
            print(f"Current outer index: {outer_index_tag}")
            print(f"Current inner index: {inner_index}")
            print(f"Addr queue: {self.addr_q}")
        # Now go through the wcb until we find a tag that matches, then read from that one
        found_match = False
        search_idx = 0
        while found_match is False:
            if self.verbose:
                print(self.wcb_tags)
            if self.wcb_tags[search_idx] == outer_index_tag:
                found_match = True
                ret_data_wide = self.wcb[search_idx]
                return ret_data_wide[inner_index]
            else:
                search_idx += 1
        # Should actually always find a value...
        return None

    def read(self, ready=False):

        if self.verbose:
            print(f"On cycle...{self.cycle}")
        self.cycle += 1

        # Address is held internally
        # Handle all the output logic...
        last_read_addr_tmp = self.last_read_addr

        pop_addr_q = False
        pop_wcb = False
        valid_out = False

        push_addr_q = False
        to_push_addr_q = 0
        to_push_pop_q = False

        data_on_sram_output_tmp = self.data_on_sram_output

        data_on_bus = data_on_sram_output_tmp
        data_being_written = False

        end_of_stream = self.get_curr_addr() is None

        # Data, valid
        return_val = (0, False)
        # If there is anything in the address queue, we have valid data at the out.
        if len(self.addr_q) > 0 and len(self.wcb) > 0:
            if self.verbose:
                print(self.wcb)
            return_val = (self.read_wcb(), True)
            valid_out = True

        else:
            return_val = (0, False)

        # We only want to pop the output queue if ready and output is valid
        if ready is True and valid_out is True:
            pop_addr_q = True
            if self.pop_q[0] and (self.wcb_num_items == 2):
                pop_wcb = True

        # We can determine if data is being written based on the pop wcb and current occupancy
        # of the WCB and if there is data on the SRAM bus
        data_being_written = data_on_bus and (pop_wcb or (self.wcb_num_items < 2))

        # Handle memory->wcb path

        if end_of_stream is False:
            # If the main memory hasn't been read yet, make sure to read it now
            # Can't return any data this cycle...
            if self.already_read is False:
                if self.verbose:
                    print("Haven't read yet...")
                self.already_read = True
                self.last_read_addr = self.get_curr_addr()
                self.next_addr()
                self.read_last_cycle = True
                self.data_on_sram_output = True

                # Will push the addr to the queue (which also means stepping the addr sequence)
                push_addr_q = True
                to_push_addr_q = self.last_read_addr
                to_push_pop_q = False

                # Always append/push
                # self.wcb.append(self.data_array[address * self.fw: address * self.fw + self.fw])

            # If it has been read, want to know if the current address goes to a new word in main memory
            elif (self.last_read_addr >> self.fw_log_2) != (self.get_curr_addr() >> self.fw_log_2):
                if self.verbose:
                    print("Outer address mismatch...")
                # CAPLIMIT: Only write the wcb if there is valid data on the sram bus AND:
                #   1. Room in the conversion buffer
                #   2. No room, but it is being popped...
                # If it's a new address, only make the read if there's room on the sram bus or it's being written
                # and there's room in the addr queue
                if ((data_on_bus is False) or data_being_written) and (len(self.addr_q) < self.addr_q_max_size):
                    self.last_read_addr = self.get_curr_addr()
                    self.next_addr()
                    self.read_last_cycle = True
                    self.data_on_sram_output = True

                    # Will push the addr to the queue (which also means stepping the addr sequence)
                    push_addr_q = True
                    to_push_addr_q = self.last_read_addr
                    to_push_pop_q = True
                else:
                    # If not enough room, can't do anything here...
                    self.read_last_cycle = False
                    # Will push the addr to the queue (which also means stepping the addr sequence)
                    push_addr_q = False
                    to_push_addr_q = 0
                    to_push_pop_q = False

            # If there's no need to read from main memory
            else:
                # Just check if room in output queues
                if len(self.addr_q) < self.addr_q_max_size:
                    self.last_read_addr = self.get_curr_addr()
                    self.next_addr()
                    # If not reading from main memory, then there is no new read to memory,
                    # but we still want to know the updated address
                    self.read_last_cycle = False
                    push_addr_q = True
                    to_push_addr_q = self.last_read_addr
                    to_push_pop_q = False
                # If not enough room in output queues, do nothing
                else:
                    # self.last_read_addr = next(self.addr_seq)
                    # If not reading from main memory, then there is no new read to memory,
                    # but we still want to know the updated address
                    self.read_last_cycle = False
                    push_addr_q = False
                    to_push_addr_q = 0
                    to_push_pop_q = False

        # If there was a read last cycle, need to push the data into the WCB
        # --- In the capacity-limited version, this is actually not true. This just means that we had a read, so there
        # will be data on the output register of the SRAM (which we are using as the skid buffer)
        # So actually is set based on if data is being written
        if data_being_written:
            # self.wcb.append(self.data_array[last_read_addr_tmp * self.fw: last_read_addr_tmp * self.fw + self.fw])
            self.wcb.append(self.data_array[(last_read_addr_tmp >> self.fw_log_2) * self.fw: (last_read_addr_tmp >> self.fw_log_2) * self.fw + self.fw])
            # self.wcb.append(self.data_array[(last_read_addr_tmp >> self.fw_log_2): (last_read_addr_tmp >> self.fw_log_2) + self.fw])
            self.wcb_num_items += 1
            # Put a tag for the address to match ... everything should work out I think...

            outer_tag = self.get_outer_address(last_read_addr_tmp)
            self.wcb_tags.append(outer_tag)
            # self.wcb_tags.append((last_read_addr_tmp >> self.fw_log_2) & 1)

        # The data on sram flag is basically
        # Will get set to true when there is a read.
        # Can only get set to false when it is written to the wcb and no follow up read
        if data_being_written and not self.read_last_cycle:
            self.data_on_sram_output = False

        # End of cycle need to do popping
        if pop_wcb:
            self.wcb.pop(0)
            self.wcb_tags.pop(0)
            self.wcb_num_items -= 1

        if pop_addr_q:
            self.addr_q.pop(0)
            self.pop_q.pop(0)

        if push_addr_q:
            self.addr_q.append(to_push_addr_q)
            self.pop_q.append(to_push_pop_q)
        else:
            self.read_last_cycle = False

        assert self.wcb_num_items <= 2
        if self.verbose:
            print(f"WCB NUM ITEMS: {self.wcb_num_items}")

        return return_val
        # return self.data_array[address * self.fw: address * self.fw + self.fw]

    def dump_mem(self):
        print(self.data_array)

    def set_addr_seq(self, addr_seq):
        self.addr_seq = addr_seq
        self.addr_seq_set = True


def convert_to_wide(thin, fw):

    # For each element, we need to break it up
    len_thin = len(thin)

    full_wide = []

    num_packed = ceil(len_thin / fw)

    for i_ in range(num_packed):
        full_wide.append(thin[i_ * fw: i_ * fw + fw])

    # print(full_wide)

    return full_wide


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Push cache model...')
    parser.add_argument('--address_space', type=int, default=64)
    parser.add_argument('--fetch_width', type=int, default=4)
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()

    fw = args.fetch_width
    address_space = args.address_space
    verbose = args.verbose

    write_data_thin = [i for i in range(address_space)]

    # print(f"Thin data: {write_data_thin}")
    write_data_wide = convert_to_wide(write_data_thin, fw=fw)
    # print(f"Wide data: {write_data_wide}")

    main_memory = MainMemoryModel(fetch_width=fw, capacity=4096, verbose=verbose)

    for i_ in range(len(write_data_wide)):
        main_memory.write(write_data_wide[i_], address=i_)

    # main_memory.dump_mem()

    read_data = []

    # make conv address
    # addr_seq = range(address_space)
    base_conv = [0, 1, 2, 3]
    addr_seq = [base_conv[z] + x for x in range(100) for z in range(len(base_conv))]
    end_idx = addr_seq.index(address_space)
    addr_seq = addr_seq[0:end_idx]
    # addr_seq_iter = iter(addr_seq)
    # addr_seq_list = [i for i in range(address_space)]

    print(addr_seq)

    main_memory.set_addr_seq(addr_seq=addr_seq)

    gold_data = [write_data_thin[x] for x in addr_seq]
    print(gold_data)

    # Need to do cycle-approximate simulation here...
    num_reads = 0
    while num_reads < len(gold_data):
        ready_choice = True
        read_result_data, read_result_valid = main_memory.read(ready_choice)
        if verbose:
            print(f"Ready: {ready_choice}\tValid: {read_result_valid}\tData: {read_result_data}")
        if ready_choice is True and read_result_valid is True:
            read_data.append(read_result_data)
            num_reads += 1

    if verbose:
        print(f"Gold data: {gold_data}")
        print(f"Read data: {read_data}")

    if gold_data == read_data:
        print(f"SUCCESS: Simulation result matches gold")
    elif len(gold_data) != len(read_data):
        print(f"Data length mismatch --- Gold length: {len(gold_data)}\tSimulation length: {len(read_data)}")
    else:
        for i in range(len(read_data)):
            if gold_data[i] != read_data[i]:
                print(f"MISMATCH\tINDEX: {i}\tGOLD: {gold_data[i]}\tSIM: {read_data[i]}")
