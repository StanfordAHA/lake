from lake.models.model import Model


class DemuxReadsModel(Model):
    '''
    Model for agg aligner
    '''
    def __init__(self,
                 fetch_width,
                 data_width,
                 banks,
                 int_out_ports):

        self.fetch_width = fetch_width
        self.data_width = data_width
        self.fw_int = int(fetch_width / data_width)
        self.banks = banks
        self.int_out_ports = int_out_ports

    # Doesn't actually have configuration state
    def set_config(self, new_config):
        return

    def interact(self, data_in, valid_in, port_in, mem_valid_data):
        '''
        Returns (data_out, valid_out, mem_valid_data_out)
        '''
        data_out = []
        valid_out = []
        mem_valid_data_out = []
        for i in range(self.int_out_ports):
            row = []
            for j in range(self.fw_int):
                row.append(0)
            data_out.append(list(row))
            valid_out.append(0)
            mem_valid_data_out.append(0)

        no_valid = True
        for i in range(self.banks):
            if(valid_in[i]):
                no_valid = False

        if no_valid:
            return (data_out, valid_out, mem_valid_data_out)

        for i in range(self.int_out_ports):
            for j in range(self.banks):
                if(valid_in[j] & (port_in[j] == (1 << i))):
                    data_out[i] = list(data_in[j])
                    valid_out[i] = 1
                    mem_valid_data_out[i] = mem_valid_data[j]
                    break

        return (data_out, valid_out, mem_valid_data_out)
