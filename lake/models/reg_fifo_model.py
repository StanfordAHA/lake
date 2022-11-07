from lake.models.model import Model


class RegFIFOModel(Model):
    '''
    Model for agg aligner
    '''
    def __init__(self,
                 data_width,
                 width_mult,
                 depth,
                 passthrough=True):

        self.data_width = data_width
        self.width_mult = width_mult
        self.depth = depth
        self.passthrough = passthrough

        # for passthrough == False
        self.delayed_push = False
        self.delayed_data_in = 0
        # only push is delayed by 1 step
        self.delayed_num_items = 0

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
            self.mvd_array.append(0)

        self.full = 0
        self.empty = 1

    # Doesn't have configuration state
    def set_config(self, new_config):
        return

    def get_full(self):
        self.full = self.num_items == self.depth
        return self.full

    def get_empty(self):
        self.empty = self.num_items == 0
        return self.empty

    def increment_rd(self):
        self.rd_ptr += 1
        if (self.rd_ptr == self.depth):
            self.rd_ptr = 0

    def increment_wr(self):
        self.wr_ptr += 1
        if (self.wr_ptr == self.depth):
            self.wr_ptr = 0

    # Assume full + empty already obeyed
    def interact(self, push, pop, data_in, mem_valid_data):
        '''
        Returns (data_out, valid, empty, full, mem_valid_data_out)
        '''

        incr_delayed_num_items = False
        if self.delayed_push:
            self.reg_array[self.wr_ptr] = self.delayed_data_in
            self.delayed_push = False
            self.increment_wr()
            incr_delayed_num_items = True

        if push and pop:
            # Push and pop on empty passes through
            # supports push and pop when full
            if self.passthrough:
                self.reg_array[self.wr_ptr] = list(data_in)
                self.increment_wr()

                if (self.num_items != 0):
                    self.increment_rd()
                else:
                    self.num_items += 1
            else:
                self.delayed_push = True
                self.delayed_data_in = list(data_in)

                if (self.delayed_num_items != 0):
                    self.increment_rd()
                    self.delayed_num_items -= 1
                else:
                    self.num_items += 1

        elif push and not pop:
            if not self.get_full():
                # Not full, push an item
                if self.passthrough:
                    self.reg_array[self.wr_ptr] = list(data_in)
                    self.increment_wr()
                else:
                    self.delayed_push = True
                    self.delayed_data_in = list(data_in)
                self.num_items += 1
        elif not push and pop:
            if ((self.passthrough and (self.num_items == 0)) or
                    (not self.passthrough and (self.delayed_num_items == 0))):
                pass
            else:
                self.increment_rd()
                self.num_items -= 1
                if not self.passthrough:
                    self.delayed_num_items -= 1

        # delayed num_items update in non-passthrough mode
        if incr_delayed_num_items:
            self.delayed_num_items += 1

        if self.passthrough:
            valid_out = int(self.num_items != 0)
        else:
            valid_out = int(self.delayed_num_items != 0)
        dat_out = self.reg_array[self.rd_ptr]
        empty_ret = int(self.num_items == 0)
        full_ret = int(self.num_items == self.depth)
        mem_valid_data_out = valid_out
        return (dat_out, valid_out, empty_ret, full_ret, mem_valid_data_out)
