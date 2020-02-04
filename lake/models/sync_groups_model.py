from lake.models.model import Model


class SyncGroupsModel(Model):
    def __init__(self,
                 fetch_width,
                 int_out_ports):

        self.fetch_width = fetch_width
        self.int_out_ports = int_out_ports
        self.groups = self.int_out_ports

        self.config = {}
        for i in range(self.int_out_ports):
            self.config[f'sync_group_{i}'] = 0

        self.local_gate = []
        self.local_mask = []

        self.sync_group_valid = []
        self.valid_reg = []
        self.data_reg = []
        for i in range(self.int_out_ports):
            self.sync_group_valid.append(0)
            self.valid_reg.append(0)
            self.data_reg.append(0)

    def set_config(self, new_config):
        # No configuration space
        return

    def interact(self,
                 ack_in,
                 data_in,
                 valid_in,
                 ren_in):
        '''
        Returns (data_out, valid_out, rd_sync_gate)
        '''
        valid_out = []
        data_out = []
        rd_sync_gate = []

        # Can get the valid syncs now
        # We do this by checking each groups members
        # to all have their valid reg high
        for i in range(self.groups):
            self.sync_group_valid[i] = 1
            for j in range(self.int_out_ports):
                # If any member of the group isn't valid yet, the group isn't valid
                if self.config[f"sync_group_{j}"] == i and self.valid_reg[j] == 0:
                    self.sync_group_valid[i] = 0

        # Each port gets its group's sync valid
        for i in range(self.int_out_ports):
            valid_out.append(self.sync_group_valid[self.config[f'sync_group_{i}']])
            data_out.append(self.data_reg[i])

        # Update the registered valids - we keep these around to track
        # which valids in the group already came
        for i in range(self.int_out_ports):
            if self.sync_group_valid[self.config[f"sync_group_{i}"]] == 1 or self.valid_reg[i] == 0:
                self.valid_reg[i] = valid_in[i]
                self.data_reg[i] = data_in[i]

        for i in range(self.int_out_ports):
            rd_sync_gate.append(0)

        return (data_out, valid_out, rd_sync_gate)
