from lake.models.model import Model


class PipeRegModel(Model):
    def __init__(self,
                 data_width,
                 stages):
        self.data_width = data_width
        self.stages = stages

        self.reg_stage = []
        if(self.stages > 0):
            for i in range(self.stages):
                self.reg_stage.append(0)

    # No config
    def set_config(self, new_config):
        return

    def update_data(self, data_in):
        if(self.stages == 0):
            return data_in
        else:
            data_curr = []
            for i in range(self.stages):
                data_curr.append(self.reg_stage[i])
            self.reg_stage[0] = data_in
            for i in range(self.stages - 1):
                self.reg_stage[i + 1] = data_curr[i]
            return self.reg_stage[self.stages - 1]
