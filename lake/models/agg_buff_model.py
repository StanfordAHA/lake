from lake.models.model import Model
from lake.models.agg_model import AggModel


class AggBuffModel(Model):

    def __init__(self,
                 agg_height,
                 data_width,
                 mem_width,
                 max_agg_schedule):

        self.agg_height = agg_height
        self.data_width = data_width
        self.mem_width = mem_width
        self.max_agg_sched = max_agg_schedule

        self.config = {}
        self.config['in_period'] = 0
        self.config['out_period'] = 0

        self.in_sched_ptr = 0
        self.out_sched_ptr = 0

        for i in range(self.max_agg_sched):
            self.config[f"in_sched_{i}"] = 0
            self.config[f"out_sched_{i}"] = 0

        self.aggs = []
        for i in range(self.agg_height):
            self.aggs.append(AggModel(int(self.mem_width / self.data_width)))
            self.aggs[i].set_config()

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val
        self.in_sched_ptr = 0
        self.out_sched_ptr = 0

    def insert(self, in_data, valid):
        to_insert = self.aggs[self.config[f"in_sched_{self.in_sched_ptr}"]]
        ag_valid = to_insert.insert(in_data, valid)

        if(ag_valid):
            self.in_sched_ptr += 1
            if(self.in_sched_ptr >= self.config['in_period']):
                self.in_sched_ptr = 0

    def get_valid_out(self):
        valid_check_agg = self.aggs[self.config[f"out_sched_{self.out_sched_ptr}"]]
        return valid_check_agg.get_valid_out()

    def get_item(self):
        valid_check_agg = self.aggs[self.config[f"out_sched_{self.out_sched_ptr}"]]
        self.out_sched_ptr += 1
        if(self.out_sched_ptr >= self.config['out_period']):
            self.out_sched_ptr = 0
        return valid_check_agg.get_data_out()

    def interact(self, in_data, valid, write_act, align):
        '''
            Returns (data_out, valid_out)
        '''
        in_sched_curr = self.in_sched_ptr
        out_sched_curr = self.out_sched_ptr

        agg_dat = []
        agg_valid = []
        agg_nf = []
        for i in range(self.agg_height):
            # Interact with child
            if i == self.config[f"in_sched_{in_sched_curr}"]:
                (t_ag_d, t_ag_v, t_nf) = self.aggs[i].interact(in_data, valid, align)
            else:
                (t_ag_d, t_ag_v, t_nf) = self.aggs[i].interact(0, 0, 0)
            agg_dat.append(t_ag_d)
            agg_valid.append(t_ag_v)
            agg_nf.append(t_nf)

        ret_data = agg_dat[self.config[f"out_sched_{out_sched_curr}"]]
        ret_valid = agg_valid[self.config[f"out_sched_{out_sched_curr}"]]

        next_full = agg_nf[self.config[f"in_sched_{in_sched_curr}"]]

        if(next_full == 1):
            self.in_sched_ptr += 1
            if(self.in_sched_ptr >= self.config['in_period']):
                self.in_sched_ptr = 0

        # Would be write_act
        if(ret_valid == 1):
            self.out_sched_ptr += 1
            if(self.out_sched_ptr >= self.config['out_period']):
                self.out_sched_ptr = 0

        return (ret_data, ret_valid)

    def peek(self):
        pass
