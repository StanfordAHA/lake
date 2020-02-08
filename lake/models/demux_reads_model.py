from lake.models.model import Model


class DemuxReadsModel(Model):
    '''
    Model for agg aligner
    '''
    def __init__(self,
                 fetch_width,
                 banks,
                 int_out_ports):

        self.fetch_width = fetch_width
        self.banks = banks
        self.int_out_ports = int_out_ports

    # Doesn't actually have configuration state
    def set_config(self, new_config):
        return

    def interact(self, data_in, valid_in, port_in):
        '''
        Returns (data_out, valid_out)
        '''
        data_out = []
        valid_out = []
        for i in range(self.int_out_ports):
            data_out.append([0] * self.fetch_width)
            valid_out.append(0)

        no_valid = True
        for i in range(self.banks):
            if(valid_in[i]):
                no_valid = False

        if no_valid:
            return (data_out, valid_out)

        for i in range(self.int_out_ports):
            for j in range(self.banks):
                if(valid_in[j] & (port_in[j] == i)):
                    data_out[i] = data_in[j]
                    valid_out[i] = 1
                    break
        return (data_out, valid_out)
