from kratos import *
import kratos as kts
from lake.attributes.config_reg_attr import ConfigRegAttr


class SyncGroups(Generator):
    '''
    Demux the SRAM reads into writes to the transpose buffers
    '''
    def __init__(self,
                 fetch_width,
                 data_width,
                 int_out_ports):

        assert not (fetch_width & (fetch_width - 1)), "Memory width needs to be a power of 2"

        super().__init__("sync_groups")
        # Absorb inputs
        self.fetch_width = fetch_width
        self.data_width = data_width
        self.fw_int = int(self.fetch_width / self.data_width)
        self.int_out_ports = int_out_ports
        self.groups = self.int_out_ports

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        self._ack_in = self.input("ack_in",
                                  self.int_out_ports)

        self._data_in = self.input("data_in",
                                   self.data_width,
                                   size=(self.int_out_ports,
                                         self.fw_int),
                                   explicit_array=True,
                                   packed=True)

        self._mem_valid_data = self.input("mem_valid_data",
                                          self.int_out_ports)

        self._mem_valid_data_out = self.output("mem_valid_data_out",
                                               self.int_out_ports)

        self._valid_in = self.input("valid_in",
                                    self.int_out_ports)
        # Indicates which port belongs to which synchronization group
        self._sync_group = self.input("sync_group",
                                      self.int_out_ports,
                                      size=self.int_out_ports,
                                      explicit_array=True,
                                      packed=True)
        sync_config = ConfigRegAttr("This array of one hot vectors" +
                                    " is used to denote which ports are synchronized to eachother." +
                                    " If multiple ports should output data relative to eachother" +
                                    " one should put them in the same group.")
        self._sync_group.add_attribute(sync_config)

        # Outputs
        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=(self.int_out_ports,
                                           self.fw_int),
                                     explicit_array=True,
                                     packed=True)

        self._valid_out = self.output("valid_out",
                                      self.int_out_ports)

        # Locals
        self._sync_agg = self.var("sync_agg",
                                  self.int_out_ports,
                                  size=self.int_out_ports,
                                  explicit_array=True,
                                  packed=True)

        self._sync_valid = self.var("sync_valid", self.int_out_ports)
        self._data_reg = self.var("data_reg",
                                  self.data_width,
                                  size=(self.int_out_ports,
                                        self.fw_int),
                                  explicit_array=True,
                                  packed=True)

        self._valid_reg = self.var("valid_reg",
                                   self.int_out_ports)

        # This signal allows us to orchestrate the synchronization groups
        # at the output address controller
        self._ren_in = self.input("ren_in",
                                  self.int_out_ports)

        self._ren_int = self.var("ren_int",
                                 self.int_out_ports)

        self._rd_sync_gate = self.output("rd_sync_gate",
                                         self.int_out_ports)

        self._local_gate_bus = self.var("local_gate_bus",
                                        self.int_out_ports,
                                        size=self.int_out_ports,
                                        explicit_array=True,
                                        packed=True)

        self._local_gate_bus_n = self.var("local_gate_bus_n",
                                          self.int_out_ports,
                                          size=self.int_out_ports,
                                          explicit_array=True,
                                          packed=True)

        self.wire(self._local_gate_bus, ~self._local_gate_bus_n)

        self._local_gate_bus_tpose = self.var("local_gate_bus_tpose",
                                              self.int_out_ports,
                                              size=self.int_out_ports,
                                              explicit_array=True,
                                              packed=True)

        self._local_gate_reduced = self.var("local_gate_reduced",
                                            self.int_out_ports)

        self._local_gate_mask = self.var("local_gate_mask",
                                         self.int_out_ports,
                                         size=self.int_out_ports,
                                         explicit_array=True,
                                         packed=True)

        self._group_finished = self.var("group_finished",
                                        self.int_out_ports)

        self._grp_fin_large = self.var("grp_fin_large",
                                       self.int_out_ports,
                                       size=self.int_out_ports,
                                       explicit_array=True,
                                       packed=True)

        self._done = self.var("done", self.int_out_ports)
        self._done_alt = self.var("done_alt", self.int_out_ports)

        # Output data is ungated
        self.wire(self._data_out, self._data_reg)
        self.wire(self._rd_sync_gate, self._local_gate_reduced)
        # Valid requires gating based on sync_valid
        self.wire(self._ren_int, self._ren_in & self._local_gate_reduced)
        # Add Code
        self.add_code(self.set_sync_agg, unroll_for=True)
        self.add_code(self.set_sync_valid)
        for i in range(self.int_out_ports):
            self.add_code(self.set_sync_stage, idx=i)
        self.add_code(self.set_out_valid)
        self.add_code(self.set_reduce_gate)
        for i in range(self.groups):
            self.add_code(self.set_rd_gates, idx=i)
        self.add_code(self.set_tpose)
        self.add_code(self.set_finished)
        self.add_code(self.next_gate_mask, unroll_for=True)
        self.add_code(self.set_grp_fin, unroll_for=True)

    @always_comb
    def set_sync_agg(self):
        # For each bit, AND the valids of each group together
        # For bits not in the group, set to 1 in the corresponding mask
        # For the group
        for i in range(self.groups):
            # For the port
            for j in range(self.int_out_ports):
                if(self._sync_group[j] == (1 << i)):
                    self._sync_agg[i][j] = self._valid_reg[j]
                else:
                    self._sync_agg[i][j] = 1

    @always_comb
    def set_sync_valid(self):
        for i in range(self.int_out_ports):
            self._sync_valid[i] = self._sync_agg[i].r_and()

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_sync_stage(self, idx):
        # Ports
        if ~self._rst_n:
            self._data_reg[idx] = 0
            self._valid_reg[idx] = 0
            self._mem_valid_data_out[idx] = 0
        # Absorb input data if the whole group is valid
        elif (self._sync_valid & self._sync_group[idx]).r_or():
            self._data_reg[idx] = self._data_in[idx]
            self._mem_valid_data_out[idx] = self._mem_valid_data[idx]
            self._valid_reg[idx] = self._valid_in[idx]
        # Also absorb input data if not currently holding a valid
        elif ~self._valid_reg[idx]:
            self._data_reg[idx] = self._data_in[idx]
            self._mem_valid_data_out[idx] = self._mem_valid_data[idx]
            self._valid_reg[idx] = self._valid_in[idx]

    @always_comb
    def set_out_valid(self):
        for i in range(self.int_out_ports):
            # For each port, check its output valid
            # should just be the sync_valid
            self._valid_out[i] = (self._sync_valid & self._sync_group[i]).r_or()

    @always_comb
    def set_reduce_gate(self):
        # This time we mean port
        for i in range(self.int_out_ports):
            self._local_gate_reduced[i] = self._local_gate_bus_tpose[i].r_and()

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_rd_gates(self, idx):
        if ~self._rst_n:
            self._local_gate_bus_n[idx] = const(0, self.int_out_ports)
        elif self._group_finished[idx]:
            self._local_gate_bus_n[idx] = const(0, self.int_out_ports)
        # Bring this down eventually
        else:
            self._local_gate_bus_n[idx] = ~(self._local_gate_bus[idx] & self._local_gate_mask[idx])

    @always_comb
    def set_tpose(self):
        for i in range(self.int_out_ports):
            for j in range(self.groups):
                self._local_gate_bus_tpose[i][j] = self._local_gate_bus[j][i]

    @always_comb
    def set_grp_fin(self):
        # Group
        for i in range(self.groups):
            # Port
            for j in range(self.int_out_ports):
                self._grp_fin_large[i][j] = 1
                if self._sync_group[j] == (1 << i):
                    # If either the read had completed or is now being completed...
                    self._grp_fin_large[i][j] = (~self._local_gate_bus[i][j] |
                                                 ~self._local_gate_mask[i][j])

    @always_comb
    def set_finished(self):
        # Mark each group as done by reduce anding its wires
        for i in range(self.groups):
            self._group_finished[i] = self._grp_fin_large[i].r_and()

    @always_comb
    def next_gate_mask(self):
        for i in range(self.groups):
            for j in range(self.int_out_ports):
                self._local_gate_mask[i][j] = 1
                if (self._sync_group[j] == (1 << i)):
                    self._local_gate_mask[i][j] = ~(self._ren_int[j] & self._ack_in[j])


if __name__ == "__main__":
    db_dut = SyncGroups(fetch_width=64,
                        data_width=16,
                        int_out_ports=2)
    verilog(db_dut, filename="sync_groups.sv", optimize_if=False, check_flip_flop_always_ff=False)
