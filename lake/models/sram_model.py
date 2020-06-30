from lake.models.model import Model
import kratos as kts


class SRAMModel(Model):
    def __init__(self,
                 data_width,
                 width_mult,
                 depth,
                 num_tiles):
        self.data_width = data_width
        self.width_mult = width_mult
        self.depth = depth
        self.num_tiles = num_tiles
        self.address_width = kts.clog2(self.num_tiles * self.depth)

        self.chain_idx_bits = max(1, kts.clog2(num_tiles))

        self.chain_idx_tile = 0

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

        addr = addr % self.depth

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
