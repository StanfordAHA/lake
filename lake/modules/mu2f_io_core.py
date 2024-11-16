from operator import mod
import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import add_counter, safe_wire, register, sticky_flag, transform_strides_and_ranges, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO
from lake.modules.mux import Mux


class IOCore_mu2f(Generator):
    def __init__(self,
                 matrix_unit_data_width=16,
                 tile_array_data_width=17,
                 fifo_depth=2,
                 allow_bypass=False,
                 use_almost_full=False,
                 add_flush=False,
                 add_clk_en=True):

        super().__init__("mu2f_io_core", debug=True)

        self.data_width = tile_array_data_width
        self.add_clk_enable = add_clk_en
        self.add_flush = add_flush
        self.fifo_depth = fifo_depth
        self.allow_bypass = allow_bypass
        self.fifo_name_suffix = "_mu2f_iocore_nof"
        self.use_almost_full = use_almost_full

        self.total_sets = 0
        num_tracks = 5

        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))

        if self.add_clk_enable:
            self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate. MU active controls this too."))

        if self.allow_bypass:
            self._dense_bypass = self.input("dense_bypass", 1)
            self._dense_bypass.add_attribute(ConfigRegAttr("Bypass FIFOS for dense mode..."))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        
        mu_data_width = matrix_unit_data_width
        mu_tile_array_datawidth_difference = tile_array_data_width - mu_data_width
        assert mu_tile_array_datawidth_difference >=0, "Error: Matrix unit bus cannot drive CGRA bus because MU datawidth > CGRA datawidth"

        ########################################
        # FIFO ZERO
        ########################################
        # Valid in from matrix unit 
        mu2io_v_0 = self.input(f"mu2io_{mu_data_width}_0_valid", 1)
        mu2io_v_0.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        # Ready out to matrix unit 
        mu2io_r_0 = self.output(f"mu2io_{mu_data_width}_0_ready", 1)
        mu2io_r_0.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        # R-V interface with fabric 
        io2f_r_0 = self.var(f"io2f_{tile_array_data_width}_0_ready", 1)
        io2f_r_0.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
        io2f_v_0 = self.var(f"io2f_{tile_array_data_width}_0_valid", 1)
        io2f_v_0.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        mu2io_0 = self.input(f"mu2io_{mu_data_width}_0", mu_data_width, packed=True)
        mu2io_0.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
    
        io2f_0 = self.var(f"io2f_{tile_array_data_width}_0", tile_array_data_width, packed=True)
        io2f_0.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
    
        # mu2io -> io2f fifo
        mu2io_2_io2f_fifo_0 = RegFIFO(data_width=tile_array_data_width,
                                        width_mult=1,
                                        depth=self.fifo_depth,
                                        mod_name_suffix=self.fifo_name_suffix,
                                        almost_full_diff=1)

        self.add_child(f"mu2io_2_io2f_{tile_array_data_width}_0",
                        mu2io_2_io2f_fifo_0,
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        clk_en=self._clk_en,
                        push=mu2io_v_0,
                        pop=io2f_r_0)
        
        # Append 0s at MSBs if CGRA bitwidth exceeds Matrix unit bitwidth
        if mu_tile_array_datawidth_difference > 0: 
            self.wire(mu2io_2_io2f_fifo_0.ports.data_in, kts.concat(kts.const(0, mu_tile_array_datawidth_difference), mu2io_0))
        else:
            self.wire(mu2io_2_io2f_fifo_0.ports.data_in, mu2io_0)
            
        ########################################
        # END FIFO ZERO
        ########################################


        ########################################
        # FIFO ONE
        ########################################
        # Valid in from matrix unit 
        mu2io_v_1 = self.input(f"mu2io_{mu_data_width}_1_valid", 1)
        mu2io_v_1.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        # Ready out to matrix unit 
        mu2io_r_1 = self.output(f"mu2io_{mu_data_width}_1_ready", 1)
        mu2io_r_1.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        # R-V interface with fabric 
        io2f_r_1 = self.var(f"io2f_{tile_array_data_width}_1_ready", 1)
        io2f_r_1.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
        io2f_v_1 = self.var(f"io2f_{tile_array_data_width}_1_valid", 1)
        io2f_v_1.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        mu2io_1 = self.input(f"mu2io_{mu_data_width}_1", mu_data_width, packed=True)
        mu2io_1.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
    
        io2f_1 = self.var(f"io2f_{tile_array_data_width}_1", tile_array_data_width, packed=True)
        io2f_1.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
    
        # mu2io -> io2f fifo
        mu2io_2_io2f_fifo_1 = RegFIFO(data_width=tile_array_data_width,
                                        width_mult=1,
                                        depth=self.fifo_depth,
                                        mod_name_suffix=self.fifo_name_suffix,
                                        almost_full_diff=1)

        self.add_child(f"mu2io_2_io2f_{tile_array_data_width}_1",
                        mu2io_2_io2f_fifo_1,
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        clk_en=self._clk_en,
                        push=mu2io_v_1,
                        pop=io2f_r_1)
        
        # Append 0s at MSBs if CGRA bitwidth exceeds Matrix unit bitwidth
        if mu_tile_array_datawidth_difference > 0: 
            self.wire(mu2io_2_io2f_fifo_1.ports.data_in, kts.concat(kts.const(0, mu_tile_array_datawidth_difference), mu2io_1))
        else:
            self.wire(mu2io_2_io2f_fifo_1.ports.data_in, mu2io_1)

        ########################################
        # END FIFO ONE
        ########################################


        #TODO: This could potentially be replaced with AND gates 
        ########################################
        # READY SELECT
        ########################################

        # Create ready select muxes
        ready_mux_0 = Mux(height=num_tracks, width=1)
        self.add_child(f"ready_0_mux", ready_mux_0)

        ready_mux_1 = Mux(height=num_tracks, width=1)
        self.add_child(f"ready_1_mux", ready_mux_1)

        # Ready select config regs (the actual select signal)
        self._ready_select_0 = self.input(f"ready_select_0", 3)
        self._ready_select_0.add_attribute(ConfigRegAttr("Track select config register. Selects driver for that track."))
        self._ready_select_1 = self.input(f"ready_select_1", 3)
        self._ready_select_1.add_attribute(ConfigRegAttr("Track select config register. Selects driver for that track."))

        # Wire up the select signals 
        self.wire(ready_mux_0.ports.S, self._ready_select_0)
        self.wire(ready_mux_1.ports.S, self._ready_select_1)

        # Create readys and wire them to mux inputs 
        for track_num in range(num_tracks):
            tmp_track_out_r = self.input(f"io2f_{tile_array_data_width}_T{track_num}_ready", 1)
            tmp_track_out_r.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            self.wire(ready_mux_0.ports.I[track_num], tmp_track_out_r)
            self.wire(ready_mux_1.ports.I[track_num], tmp_track_out_r)


        self.wire(io2f_r_0, ready_mux_0.ports.O)
        self.wire(io2f_r_1, ready_mux_1.ports.O)

        # If MU_inactive, set ready_out = 0
        # If dense bypass, send data straight through, bypassing FIFOs
        if self.allow_bypass:
            if self.use_almost_full:
                self.wire(mu2io_r_0, kts.ternary(self._tile_en, kts.ternary(self._dense_bypass,
                                                    io2f_r_0,
                                                    ~mu2io_2_io2f_fifo_0.ports.almost_full)), kts.const(0, 1))
                self.wire(mu2io_r_1, kts.ternary(self._tile_en, kts.ternary(self._dense_bypass,
                                                    io2f_r_1,
                                                    ~mu2io_2_io2f_fifo_1.ports.almost_full)), kts.const(0, 1))
            else:
                self.wire(mu2io_r_0, kts.ternary(self._tile_en, kts.ternary(self._dense_bypass,
                                                    io2f_r_0,
                                                    ~mu2io_2_io2f_fifo_0.ports.full)), kts.const(0, 1))
                self.wire(mu2io_r_1, kts.ternary(self._tile_en, kts.ternary(self._dense_bypass,
                                                    io2f_r_1,
                                                    ~mu2io_2_io2f_fifo_1.ports.full)), kts.const(0, 1))
        else:
            if self.use_almost_full:
                self.wire(mu2io_r_0, kts.ternary(self._tile_en, ~mu2io_2_io2f_fifo_0.ports.almost_full, kts.const(0, 1)))
                self.wire(mu2io_r_1, kts.ternary(self._tile_en, ~mu2io_2_io2f_fifo_1.ports.almost_full, kts.const(0, 1)))
            else:
                self.wire(mu2io_r_0, kts.ternary(self._tile_en, ~mu2io_2_io2f_fifo_0.ports.full, kts.const(0, 1)))
                self.wire(mu2io_r_1, kts.ternary(self._tile_en, ~mu2io_2_io2f_fifo_1.ports.full, kts.const(0, 1)))



        ########################################
        # TRACK SELECT 
        ########################################
        for track_num in range(num_tracks):
            # Create track select config reg
            self._tmp_track_select = self.input(f"track_select_T{track_num}", 2)
            self._tmp_track_select.add_attribute(ConfigRegAttr("Track select config register. Selects driver for that track."))

            # Create track output and its valid interface 
            tmp_track_out = self.output(f"io2f_{tile_array_data_width}_T{track_num}", tile_array_data_width)
            tmp_track_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_track_out_v = self.output(f"io2f_{tile_array_data_width}_T{track_num}_valid", 1)
            tmp_track_out_v.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))


            # Create 3-to-1 mux (track_select mux)
            track_mux = Mux(height=3, width=tile_array_data_width)
            self.add_child(f"T_{track_num}_mux", track_mux)

            # Create 3-to-1 mux (track_select valid mux)
            track_valid_mux = Mux(height=3, width=1)
            self.add_child(f"T_{track_num}_valid_mux", track_valid_mux)

            # Wire track select signal 
            self.wire(track_mux.ports.S, self._tmp_track_select)
            self.wire(track_valid_mux.ports.S, self._tmp_track_select)

            self.wire(track_mux.ports.I[0], kts.const(0, tile_array_data_width))
            self.wire(track_valid_mux.ports.I[0], kts.const(0, 1))

            # FIFO -> 3-to-1 mux connnections 
            if self.allow_bypass:
                self.wire(track_mux.ports.I[1], kts.ternary(self._dense_bypass, mu2io_0, mu2io_2_io2f_fifo_0.ports.data_out))
                self.wire(track_mux.ports.I[2], kts.ternary(self._dense_bypass, mu2io_1, mu2io_2_io2f_fifo_1.ports.data_out))

                self.wire(track_valid_mux.ports.I[1], kts.ternary(self._dense_bypass, mu2io_v_0, ~mu2io_2_io2f_fifo_0.ports.empty))
                self.wire(track_valid_mux.ports.I[2], kts.ternary(self._dense_bypass, mu2io_v_1, ~mu2io_2_io2f_fifo_1.ports.empty))
            else:
                self.wire(track_mux.ports.I[1], mu2io_2_io2f_fifo_0.ports.data_out)
                self.wire(track_mux.ports.I[2], mu2io_2_io2f_fifo_1.ports.data_out)

                self.wire(track_valid_mux.ports.I[1], ~mu2io_2_io2f_fifo_0.ports.empty)
                self.wire(track_valid_mux.ports.I[2], ~mu2io_2_io2f_fifo_1.ports.empty)

           
            # MU active mux (wire output)
            self.wire(tmp_track_out, kts.ternary(self._tile_en, track_mux.ports.O, kts.const(0, 1)))
            self.wire(tmp_track_out_v, kts.ternary(self._tile_en, track_valid_mux.ports.O, kts.const(0, 1)))


        if self.add_clk_enable:
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))

        if self.add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

    def get_bitstream(self, config_dict):

        # Store all configurations here
        config = [("tile_en", 1)]

        track_select_T0_val = 0
        track_select_T1_val = 0
        track_select_T2_val = 0
        track_select_T3_val = 0 
        track_select_T4_val = 0

        if 'track_select_T0' in config_dict:
            track_select_T0_val = config_dict['track_select_T0']

        if 'track_select_T1' in config_dict:
            track_select_T1_val = config_dict['track_select_T1']

        if 'track_select_T2' in config_dict:
            track_select_T2_val = config_dict['track_select_T2']
        
        if 'track_select_T3' in config_dict:
            track_select_T3_val = config_dict['track_select_T3']

        if 'track_select_T4' in config_dict:
            track_select_T4_val = config_dict['track_select_T4']


        config += [("track_select_T0", track_select_T0_val), ("track_select_T1", track_select_T1_val),
                    ("track_select_T2", track_select_T2_val), ("track_select_T3", track_select_T3_val), 
                    ("track_select_T4", track_select_T4_val)]

    
        ready_select_0_val = 0
        ready_select_1_val = 0

        if 'ready_select_0' in config_dict:
            ready_select_0_val = config_dict['ready_select_0']

        if 'ready_select_1' in config_dict:
            ready_select_1_val = config_dict['ready_select_1']

        config += [("ready_select_0", ready_select_0_val), ("ready_select_1", ready_select_1_val)]

        if self.allow_bypass:

            dense_bypass_val = 0

            if 'dense_bypass' in config_dict:
                dense_bypass_val = config_dict['dense_bypass']

            config += [("dense_bypass", dense_bypass_val)]

        return config


if __name__ == "__main__":

    io_core_dut = IOCore_mu2f(tile_array_data_width=17, matrix_unit_data_width=16, allow_bypass=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")
    verilog(io_core_dut, filename="mu2f_iocore.sv",
            optimize_if=False)
