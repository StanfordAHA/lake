from lake.models.model import Model


class RegFIFOModel(Model):
    '''
    Model for agg aligner
    '''
    def __init__(self,
                 data_width,
                 width_mult,
                 depth):

        self.data_width = data_width
        self.width_mult = width_mult
        self.depth = depth

        self.config = {}

        self.rd_ptr = 0
        self.wr_ptr = 0

        self.num_items = 0
        self.reg_array = []
        self.mvd_array = []
        for i in range(self.depth):
            row = []
            for j in range(self.width_mult):
                row.append(0)
            self.reg_array.append(row)
            self.mvd_array.append(row)

        self.full = 0
        self.empty = 1

    # Doesn't have configuration state
    def set_config(self, new_config):
        return

    def get_full(self, push, pop):
        self.full = self.num_items == self.depth
        return self.full

    def get_empty(self, push, pop):
        self.empty = self.num_items == 0
        return self.empty

    def increment_rd(self):
        self.rd_ptr += 1
        if(self.rd_ptr == self.depth):
            self.rd_ptr = 0

    def increment_wr(self):
        self.wr_ptr += 1
        if(self.wr_ptr == self.depth):
            self.wr_ptr = 0

    # Assume full + empty already obeyed
    def interact(self, push, pop, data_in, mem_valid_data):
        '''
        Returns (data_out, valid, empty, full, mem_valid_data_out)
        '''
        empty_ret = int(self.num_items == 0)
        full_ret = int(self.num_items == self.depth)

        if push and pop:
            # Push and pop on empty passes through
            if(self.num_items == 0):
                return (data_in, 1, empty_ret, full_ret, mem_valid_data)
            else:
                dat_out = self.reg_array[self.rd_ptr]
                mem_valid_data_out = self.mvd_array[self.rd_ptr]
                self.increment_rd()
                self.reg_array[self.wr_ptr] = list(data_in)
                self.mvd_array[self.wr_ptr] = mem_valid_data
                self.increment_wr()
                return (dat_out, 1, empty_ret, full_ret, mem_valid_data_out)
        elif push and not pop:
            # Not full, push an item
            if(self.num_items == self.depth):
                return ([0], 0, empty_ret, full_ret, 0)
            self.reg_array[self.wr_ptr] = list(data_in)
            self.mvd_array[self.wr_ptr] = mem_valid_data
            self.increment_wr()
            self.num_items += 1
            # return (self.reg_array[self.rd_ptr], 0, empty_ret, full_ret)
            return ([0], 0, empty_ret, full_ret, 0)
        elif not push and pop:
            if(self.num_items == 0):
                # return (self.reg_array[self.rd_ptr], 0, empty_ret, full_ret)
                return ([0], 0, empty_ret, full_ret, 0)
            dat_out = self.reg_array[self.rd_ptr]
            mem_valid_data_out = self.mvd_array[self.rd_ptr]
            self.increment_rd()
            self.num_items -= 1
            return (dat_out, 1, empty_ret, full_ret, mem_valid_data_out)
        else:
            # return (self.reg_array[self.rd_ptr], 0, empty_ret, full_ret)
            return ([0], 0, empty_ret, full_ret, 0)
