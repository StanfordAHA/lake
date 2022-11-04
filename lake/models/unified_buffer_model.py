from lake.models.model import Model
import kratos as kts


class UBModel(Model):
    def __init__(self,
                 in_width,
                 out_width,
                 mem_width,
                 mem_depth):
        self.in_width = in_width
        self.out_width = out_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth

        self.rd_reg = []
        for i in range(self.out_width):
            self.rd_reg.append(0)

        self.mem = []
        for i in range(self.mem_depth):
            if self.mem_width != 1:
                row = []
                for j in range(self.mem_width):
                    row.append(0)
                self.mem.append(row)
            else:
                self.mem.append(0)

    def interact(self,
                 wen,
                 ren,
                 addr,
                 data):
        '''
        Returns (rd_reg)
        '''

        # take only 9 bit when overflow
        if wen:     # write
            addr = addr % (self.mem_depth // self.in_width)
            addr = addr * self.in_width
            if self.in_width != 1:
                for a in range(addr, addr + self.in_width):
                    self.mem[a] = data[a - addr]
            else:
                self.mem[addr] = data

        elif ren:   # read
            addr = addr % (self.mem_depth // self.out_width)
            addr = addr * self.out_width
            if self.out_width != 1:
                rd_reg = []
                for a in range(addr, addr + self.out_width):
                    rd_reg.append(self.mem[a])
            else:
                rd_reg = self.mem[addr]
            self.rd_reg = rd_reg

        return self.rd_reg

    def get_rd_reg(self):
        return list(self.rd_reg)

    def dump_mem(self):
        for i in range(self.mem_depth):
            print(f"addr: {i}, data: {self.mem[i]}")
