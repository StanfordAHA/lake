from lake.models.model import Model
from lake.models.agg_model import AggModel

class AggBuffModel(Model):

    def __init__(self, agg_height, data_width, mem_width, max_agg_schedule):
        self.agg_height = agg_height
        self.data_width = data_width
        self.mem_width = mem_width
        self.max_agg_sched = max_agg_schedule

        self.in_sched = []
        self.in_sched_ptr = 0
        self.out_sched = []
        self.out_sched_ptr = 0

        for i in range(self.max_agg_sched):
            self.in_sched.append(0)
            self.out_sched.append(0)

        self.in_period = 0
        self.out_period = 0

        self.aggs = []
        for i in range(self.agg_height):
            self.aggs.append(AggModel(int(self.mem_width/self.data_width)))
            self.aggs[i].set_config()
        