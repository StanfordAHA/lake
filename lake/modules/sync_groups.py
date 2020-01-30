from kratos import *
import kratos as kts
from lake.attributes.config_reg_attr import ConfigRegAttr


class SyncGroups(Generator):
    '''
    Demux the SRAM reads into writes to the transpose buffers
    '''
    def __init__(self,
                 fetch_width,
                 int_out_ports):

        assert not (fetch_width & (fetch_width - 1)), "Memory width needs to be a power of 2"

        super().__init__("sync_groups", debug=True)
        # Absorb inputs
        self.fetch_width = fetch_width
        self.int_out_ports = int_out_ports

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        self._ack_in = self.input("ack_in",
                                  self.int_out_ports)

        self._data_in = self.input("data_in",
                                   self.fetch_width,
                                   size=self.int_out_ports,
                                   explicit_array=True,
                                   packed=True)

        self._valid_in = self.input("valid_in",
                                    self.int_out_ports)
        # Indicates which port belongs to which synchronization group
        self._sync_group = self.input("sync_group",  # CONFIG REG
                                      self.int_out_ports,
                                      size=self.int_out_ports,
                                      explicit_array=True,
                                      packed=True)
        self._sync_group.add_attribute(ConfigRegAttr())

        # Outputs
        self._data_out = self.output("data_out",
                                     self.fetch_width,
                                     size=self.int_out_ports,
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
        self._data_sync = self.var("data_sync",
                                   self.fetch_width,
                                   size=self.int_out_ports,
                                   explicit_array=True,
                                   packed=True)

        self._valid_sync = self.var("valid_sync",
                                    self.int_out_ports)

        # This signal allows us to orchestrate the synchronization groups
        # at the output address controller
        self._ren_in = self.input("ren_in",
                                  self.int_out_ports)

        self._rd_sync_gate = self.output("rd_sync_gate",
                                         self.int_out_ports)

        self._local_gate_bus = self.var("local_gate_bus",
                                        self.int_out_ports,
                                        size=self.int_out_ports,
                                        explicit_array=True,
                                        packed=True)

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

        self._lowest_in_group = self.var("lowest_in_group",
                                         self.int_out_ports,
                                         size=self.int_out_ports,
                                         explicit_array=True,
                                         packed=True)

        self._group_finished = self.var("group_finished",
                                        self.int_out_ports)

        self._done = self.var("done", self.int_out_ports)

        # Output data is ungated
        self.wire(self._data_out, self._data_sync)
        self.wire(self._rd_sync_gate, self._local_gate_reduced)
        # Valid requires gating based on sync_valid

        # Vars

        self.add_code(self.set_sync_agg)
        self.add_code(self.set_sync_valid)
        self.add_code(self.set_sync_stage)
        self.add_code(self.set_out_valid)
        self.add_code(self.set_reduce_gate)
        self.add_code(self.set_rd_gates)
        self.add_code(self.set_lowest_in_grp)
        self.add_code(self.set_tpose)

    @always_comb
    def set_sync_agg(self):
        # For each bit, AND the valids of each group together
        # For bits not in the group, set to 1 in the corresponding mask
        # For the group
        for i in range(self.int_out_ports):
            # For the port
            for j in range(self.int_out_ports):
                if(self._sync_group[j] == (1 << i)):
                    self._sync_agg[i][j] = self._valid_sync[j]
                else:
                    self._sync_agg[i][j] = 1

    @always_comb
    def set_sync_valid(self):
        for i in range(self.int_out_ports):
            self._sync_valid[i] = self._sync_agg[i].r_and()

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_sync_stage(self):
        for i in range(self.int_out_ports):
            if ~self._rst_n:
                self._data_sync[i] = 0
                self._valid_sync[i] = 0
            # Absorb input data if the whole group is valid
            elif (self._sync_valid & self._sync_group[i]).r_or():
                self._data_sync[i] = self._data_in[i]
                self._valid_sync[i] = self._valid_in[i]
            # Also absorb input data if not currently holding a valid
            elif ~self._valid_sync[i]:
                self._data_sync[i] = self._data_in[i]
                self._valid_sync[i] = self._valid_in[i]

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
    def set_rd_gates(self):
        for i in range(self.int_out_ports):
            if ~self._rst_n | self._group_finished[i]:
                self._local_gate_bus[i] = ~const(0, self.int_out_ports)
            # Bring this down eventually
            else:
                self._local_gate_bus[i] = self._local_gate_bus[i] & self._local_gate_mask[i]

    @always_comb
    def set_lowest_in_grp(self):
        # For each GROUP
        for i in range(self.int_out_ports):
            self._lowest_in_group[i] = 0
            self._done[i] = 0
            for j in range(self.int_out_ports):
                if ~self._done[i] & (self._sync_group[j] == (1 << i)):
                    if self._local_gate_bus[i]:
                        self._done[i] = 1
                        self._lowest_in_group[i] = j

    @always_comb
    def set_tpose(self):
        for i in range(self.int_out_ports):
            for j in range(self.int_out_ports):
                self._local_gate_bus_tpose[i][j] = self._local_gate_bus[j][i]

    @always_comb
    def next_gate_mask(self):
        for i in range(self.int_out_ports):
            for j in range(self.int_out_ports):
                self._local_gate_mask[i][j] = 1
                if self._lowest_in_group[i] == j:
                    self._local_gate_mask[i][j] = ~(self._ren_in[j] & self._ack_in[j])


if __name__ == "__main__":
    db_dut = SyncGroups(fetch_width=64,
                        int_out_ports=2)

    verilog(db_dut, filename="sync_groups.sv", optimize_if=False)
