from lake.models.model import Model
import kratos as kts


class SyncGroupsModel(Model):
    def __init__(self,
                 fetch_width,
                 data_width,
                 int_out_ports):

        self.fetch_width = fetch_width
        self.data_width = data_width
        self.fw_int = int(self.fetch_width / self.data_width)
        self.int_out_ports = int_out_ports
        self.groups = self.int_out_ports

        self.config = {}
        for i in range(self.int_out_ports):
            self.config[f'sync_group_{i}'] = 0

        self.local_gate = []
        self.local_mask = []

        for i in range(self.groups):
            self.local_gate.append([])
            self.local_mask.append([])
            for j in range(self.int_out_ports):
                self.local_gate[i].append(1)
                self.local_mask[i].append(1)

        self.local_gate_reduced = []
        self.sync_group_valid = []
        self.valid_reg = []
        self.data_reg = []
        self.mem_valid_data_out_reg = []
        for i in range(self.int_out_ports):
            self.local_gate_reduced.append(1)
            self.sync_group_valid.append(0)
            self.valid_reg.append(0)
            self.mem_valid_data_out_reg.append(0)
            row = []
            for j in range(self.fetch_width):
                row.append(0)
            self.data_reg.append(row)

    def set_config(self, new_config):
        # No configuration space
        # Configure top level
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

    def interact(self,
                 ack_in,
                 data_in,
                 valid_in,
                 ren_in,
                 mem_valid_data):
        '''
        Returns (data_out, valid_out, rd_sync_gate, mem_valid_data_out)
        '''
        valid_out = []
        data_out = []
        rd_sync_gate = []
        mem_valid_data_out = []
        # # Use current state of bus to set local gate reduced
        # for i in range(self.int_out_ports):
        #     # For this port, just want to check that its corresponding entry is low
        #     for j in range(self.groups):
        #         if self.config[f"sync_group_{i}"] == (1 << j):
        #             self.local_gate_reduced[i] = self.local_gate[j][i]

        # Set the ren_int, ack_in combo
        ren_int = []
        for i in range(self.int_out_ports):
            ren_int.append(ren_in[i] & self.local_gate_reduced[i])

        # Create new local mask
        for i in range(self.groups):
            for j in range(self.int_out_ports):
                self.local_mask[i][j] = 1
                # If port j is in group i, set its gate mask
                if self.config[f"sync_group_{j}"] == (1 << i):
                    self.local_mask[i][j] = not (ren_int[j] & ((ack_in & (1 << j)) != 0))
                    # self.local_mask[i][j] = not (ren_int[j] and ack_in[j])

        # Get group finished
        group_finished = []
        for i in range(self.groups):
            group_finished.append(1)
            # Check that either the bus or mask is low for all items in the group
            for j in range(self.int_out_ports):
                # Only check if the port is in the group
                if self.config[f"sync_group_{j}"] == (1 << i):
                    if self.local_gate[i][j] == 1 and self.local_mask[i][j] == 1:
                        group_finished[i] = 0

        rd_sync_gate = self.get_rd_sync()

        for i in range(self.groups):
            for j in range(self.int_out_ports):
                if group_finished[i] == 1:
                    self.local_gate[i][j] = 1
                else:
                    self.local_gate[i][j] = self.local_gate[i][j] & self.local_mask[i][j]

        # Can get the valid syncs now
        # We do this by checking each groups members
        # to all have their valid reg high
        for i in range(self.groups):
            self.sync_group_valid[i] = 1
            for j in range(self.int_out_ports):
                # If any member of the group isn't valid yet, the group isn't valid
                if (self.config[f"sync_group_{j}"] == (1 << i)) and self.valid_reg[j] == 0:
                    self.sync_group_valid[i] = 0

        # Each port gets its group's sync valid
        for i in range(self.int_out_ports):
            valid_out.append(self.sync_group_valid[kts.clog2(self.config[f'sync_group_{i}'])])
            # valid_out.append(self.sync_group_valid[self.config[f'sync_group_{i}']])
            data_out.append(self.data_reg[i].copy())
            mem_valid_data_out.append(self.mem_valid_data_out_reg[i])

        # Update the registered valids - we keep these around to track
        # which valids in the group already came
        for i in range(self.int_out_ports):
            group_log = kts.clog2(self.config[f"sync_group_{i}"])
            if self.sync_group_valid[group_log] == 1 or self.valid_reg[i] == 0:
                self.valid_reg[i] = valid_in[i]
                self.data_reg[i] = data_in[i].copy()
                self.mem_valid_data_out_reg[i] = mem_valid_data[i]

        # Use current state of bus to set local gate reduced
        for i in range(self.int_out_ports):
            # For this port, just want to check that its corresponding entry is low
            for j in range(self.groups):
                if self.config[f"sync_group_{i}"] == (1 << j):
                    self.local_gate_reduced[i] = self.local_gate[j][i]

        # rd_sync_gate = self.get_rd_sync()
        return (data_out.copy(), valid_out, rd_sync_gate.copy(), mem_valid_data_out)

    def get_rd_sync(self):
        rd_sync_gate = []
        for i in range(self.int_out_ports):
            rd_sync_gate.append(self.local_gate_reduced[i])
        return list(rd_sync_gate)
