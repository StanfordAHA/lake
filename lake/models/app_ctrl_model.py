from lake.models.model import Model


class AppCtrlModel(Model):
    '''
    Model for app ctrl
    '''
    def __init__(self,
                 int_in_ports,
                 int_out_ports,
                 sprt_stcl_valid=False):

        self.int_in_ports = int_in_ports
        self.int_out_ports = int_out_ports

        self.config = {}
        for i in range(self.int_in_ports):
            self.config[f'write_depth_{i}'] = 0
        for i in range(self.int_out_ports):
            self.config[f'read_depth_{i}'] = 0
            self.config[f'input_port_{i}'] = 0
            self.config[f'prefill_{i}'] = 0

        self.write_count = [0] * self.int_in_ports
        self.write_done_d = [0] * self.int_in_ports
        self.read_count = [0] * self.int_out_ports
        self.init_state = [0] * self.int_out_ports
        self.read_done_d = [0] * self.int_out_ports

    # Doesn't actually have configuration state
    def set_config(self, new_config):
        # No configuration space
        # Configure top level
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

    def interact(self, wen_in, ren_in, tb_valid, ren_update):
        '''
        Returns (wen_out, ren_out, valid_out)
        '''
        print(wen_in)
        print(ren_in)
        print(tb_valid)
        print(ren_update)
        print(self.config)
        print()

        valid_out_data = tb_valid.copy()
        valid_out_stencil = tb_valid.copy()

        curr_read_done_d = self.read_done_d
        curr_write_done_d = self.write_done_d

        wen_out = []
        for i in range(self.int_in_ports):
            wen_out.append((self.write_done_d[i] == 0) and (wen_in[i] == 1))

        ren_out = []
        for i in range(self.int_out_ports):
            ren_out.append((self.read_done_d[i] == 0) and
                           (ren_in[i] == 1) and
                           ((self.config[f'prefill_{i}'] == 1) or (self.init_state[i] == 1)))

        write_done = []
        for i in range(self.int_in_ports):
            wd_temp = (((wen_in[i] == 1) and (self.write_count[i] == self.config[f'write_depth_{i}'] - 1)) or
                       self.write_done_d[i] == 1)
            write_done.append(wd_temp)

        read_done = []
        for i in range(self.int_out_ports):
            rd_temp = (((ren_update[i] == 1) and
                        (ren_in[i] == 1) and
                        (self.read_count[i] == self.config[f'read_depth_{i}'] - 1)) or
                       curr_read_done_d[i] == 1 or
                       ((self.config[f'prefill_{i}'] == 0) and (self.init_state[i] == 0)))
            read_done.append(rd_temp)

        # read_done_ff
        for i in range(self.int_out_ports):
            if (write_done[self.config[f"input_port_{i}"]] == 1) and (read_done[i] == 1):
                self.read_done_d[i] = 0
            elif read_done[i] == 1:
                self.read_done_d[i] = 1

        # Deal with write done ff
        for i in range(self.int_in_ports):
            if (write_done[i] == 1) and (read_done[i] == 1):
                self.write_done_d[i] = 0
            elif write_done[i] == 1:
                self.write_done_d[i] = 1

        # Init state
        for i in range(self.int_out_ports):
            if write_done[self.config[f"input_port_{i}"]] == 1:
                self.init_state[i] = 1

        # write count
        for i in range(self.int_in_ports):
            if (write_done[i] == 1) and (read_done[i] == 1):
                self.write_count[i] = 0
            elif (wen_in[i] == 1) and (curr_write_done_d[i] == 0):
                self.write_count[i] += 1

        # read count
        for i in range(self.int_out_ports):
            if (write_done[self.config[f"input_port_{i}"]] == 1) and (read_done[i] == 1):
                self.read_count[i] = 0
            elif (ren_in[i] == 1) & (ren_update[i] == 1):
                self.read_count[i] += 1

        print(wen_out, " ", ren_out, " ", valid_out_data)
        print(self.read_done_d)
        print(read_done)
        print(write_done[self.config[f"input_port_0"]])
        print(self.read_count)
        print(self.init_state)
        print()
        return (wen_out, ren_out, valid_out_data, valid_out_stencil)
