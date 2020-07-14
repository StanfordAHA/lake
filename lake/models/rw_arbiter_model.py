from lake.models.model import Model


class RWArbiterModel(Model):
    def __init__(self,
                 fetch_width,
                 data_width,
                 memory_depth,
                 int_out_ports,
                 read_delay):
        self.fetch_width = fetch_width
        self.memory_depth = memory_depth
        self.int_out_ports = int_out_ports
        self.read_delay = read_delay

        self.rd_valid = 0
        self.rd_port = 0
        self.ack = 0

    def set_config(self, new_config):
        # No configuration space
        return

    def interact(self,
                 wen_in,
                 wen_en,
                 w_data,
                 w_addr,
                 data_from_mem,
                 ren_in,
                 ren_en,
                 rd_addr,
                 mem_valid_data):
        '''
        Returns (out_dat, out_port, out_valid,
                 cen_mem, wen_mem, data_to_mem, addr_to_mem, ack,
                 out_mem_valid_data)
        '''
        # These signals are always this way
        out_dat = data_from_mem
        if self.read_delay == 1:
            out_port = self.rd_port
            out_valid = self.rd_valid
        else:
            out_port = 0
            out_valid = 0
        data_to_mem = w_data
        if type(wen_en) is list:
            wen_en = wen_en[0]
        wen_mem = wen_in & wen_en
        # Signals following may vary
        cen_mem = 0
        addr_to_mem = 0

        self.rd_valid = 0
        if wen_in != 0 and wen_en != 0:
            cen_mem = 1
            addr_to_mem = w_addr
            self.rd_valid = 0
            self.rd_port = 0
        elif ren_in != 0 and ren_en != 0:
            for i in range(self.int_out_ports):
                # Select lowest
                if ren_in[i] != 0 and ren_en[i] != 0:
                    cen_mem = 1
                    addr_to_mem = rd_addr[i]
                    self.rd_valid = 1
                    self.rd_port = 1 << i
                    if self.read_delay == 0:
                        out_valid = 1
                        out_port = 1 << i
                    break
        ack = self.get_ack(wen_in, wen_en, ren_in, ren_en)

        out_mem_valid_data = mem_valid_data

        return (out_dat, out_port, out_valid,
                cen_mem, wen_mem, data_to_mem,
                addr_to_mem, ack, out_mem_valid_data)

    def get_ack(self, wen_in, wen_en, ren_in, ren_en):
        self.ack = 0
        if wen_in != 0 and wen_en != 0:
            self.ack = 0
        elif ren_in != 0 and ren_en != 0:
            for i in range(self.int_out_ports):
                # Select lowest
                if ren_in[i] != 0 and ren_en[i]:
                    self.ack = 1 << i
                    break
        return self.ack
