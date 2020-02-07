from lake.models.model import Model


class SRAMModel(Model):
    def __init__(self,
                 width,
                 depth):
        self.width = width
        self.depth = depth

        self.rd_reg = 0
        self.mem = []
        for i in range(self.depth):
            self.mem.append(0)

    def set_config(self, new_config):
        # No configuration space
        return

    def interact(self,
                 wen,
                 cen,
                 addr,
                 data):
        '''
        Returns (rd_reg)
        '''
        rd_reg_ret = self.rd_reg

        # no-op
        if cen == 0:
            return rd_reg_ret
        elif wen == 1:
            self.mem[addr] = data
        else:
            self.rd_reg = self.mem[addr]

        return rd_reg_ret

    def get_rd_reg(self):
        return self.rd_reg
