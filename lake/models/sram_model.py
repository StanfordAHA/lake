from lake.models.model import Model


class SRAMModel(Model):
    def __init__(self,
                 data_width,
                 width_mult,
                 depth):
        self.data_width = data_width
        self.width_mult = width_mult
        self.depth = depth

        self.rd_reg = []
        for i in range(self.width_mult):
            self.rd_reg.append(0)
        self.mem = []
        for i in range(self.depth):
            row = []
            for j in range(self.width_mult):
                row.append(0)
            self.mem.append(row)

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
            self.mem[addr] = data.copy()
        else:
            # Read
            self.rd_reg = self.mem[addr]

        return list(rd_reg_ret)

    def get_rd_reg(self):
        return list(self.rd_reg)

    def dump_mem(self):
        for i in range(self.depth):
            print(f"addr: {i}, data: {self.mem[i]}")
