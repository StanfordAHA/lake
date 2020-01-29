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

    def input_data(self, data_in, valid_in, port_in):
        '''
        Returns (data_out, valid_out)
        '''
        data_out = []
        valid_out = []
        no_valid = True
        for i in range(self.banks):
            if(valid_in[i]):
                no_valid = False
        
        if no_valid:
            return ()
