from lake.models.model import Model
from lake.models.reg_fifo_model import RegFIFOModel


class PrefetcherModel(Model):
    def __init__(self,
                 fetch_width,
                 data_width,
                 max_prefetch):

        self.fetch_width = fetch_width
        self.max_prefetch = max_prefetch
        self.data_width = data_width
        self.fw_int = int(self.fetch_width / self.data_width)

        self.config = {}
        self.config['input_latency'] = 0

        self.fifo = RegFIFOModel(data_width=self.data_width,
                                 width_mult=self.fw_int,
                                 depth=self.max_prefetch)

        self.cnt = 0

    def set_config(self, new_config):
        # Configure top level
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

    def update_cnt(self, valid_read, tba_rdy):
        if valid_read != 0 and tba_rdy == 0:
            self.cnt -= 1
        elif valid_read != 0 & tba_rdy != 0 and not self.fifo.get_full():
            self.cnt += 1

    def get_step(self):
        return int((self.cnt + self.config['input_latency']) < (self.max_prefetch))

    def get_cnt(self):
        return self.cnt

    def interact(self, data_in, valid_read, tba_rdy, mem_valid_data):
        '''
        Returns (data, valid, step, mem_valid_data_out)
        '''
        (d_out, v_out, empty, full, mem_valid_data_out) = \
            self.fifo.interact(valid_read, tba_rdy, data_in, mem_valid_data)
        stp = self.get_step()
        self.update_cnt(valid_read, tba_rdy)
        if type(d_out) == list:
            return (d_out.copy(), v_out, stp, mem_valid_data_out)
        else:
            return (d_out, v_out, stp, mem_valid_data_out)
