import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.utils.util import extract_formal_annotation, generate_pond_api, safe_wire, add_counter, trim_config
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.modules.register_file import RegisterFile
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.modules.storage_config_seq import StorageConfigSeq
from lake.utils.parse_clkwork_config import map_controller, extract_controller_json
from lake.utils.parse_clkwork_config import ControllerInfo
from _kratos import create_wrapper_flatten


class ValidIO(Generator):
    def __init__(self,
                 default_iterator_support=6,
                 config_data_width=32,
                 config_addr_width=8,
                 cycle_count_width=16,
                 add_clk_enable=True,
                 add_flush=True):
        super().__init__("valid_io", debug=True)

        self.config_data_width = config_data_width
        self.config_addr_width = config_addr_width
        self.add_clk_enable = add_clk_enable
        self.add_flush = add_flush
        self.cycle_count_width = cycle_count_width
        self.default_iterator_support = default_iterator_support
        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        # Gate clock if tile is disabled...
        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # Add global cycle counter...
        self._cycle_count = add_counter(self, "cycle_count", self.cycle_count_width)

        # Create valid out and generation logic for it...
        self._valid_out = self.output("valid_out", 1)
        self._valid_out.add_attribute(ControlSignalAttr(False))
        self._loops_valid_out = ForLoop(iterator_support=self.default_iterator_support,
                                        config_width=self.cycle_count_width)
        self._valid_out_int = self.var("valid_out_int", 1)
        # Loop Iterators for valid...
        self.add_child(f"loops_valid_out",
                       self._loops_valid_out,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       step=self._valid_out_int)
        # Schedule Generator for valid...
        self.add_child(f"valid_out_sched_gen",
                       SchedGen(iterator_support=self.default_iterator_support,
                                config_width=self.cycle_count_width),
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       mux_sel=self._loops_valid_out.ports.mux_sel_out,
                       valid_output=self._valid_out_int)

        # Wire out internal wire
        self.wire(self._valid_out, self._valid_out_int)

        if self.add_clk_enable:
            self.clock_en("clk_en")
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))

        if self.add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

        # Finally, lift the config regs...
        lift_config_reg(self.internal_generator)

    def get_static_bitstream_json(self,
                                  root_node):

        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Store all configurations here
        config = [
            # Activate the tile...
            trim_config(flattened, "flush_reg_sel", 0),  # 1
            trim_config(flattened, "flush_reg_value", 0),  # 1
            trim_config(flattened, "tile_en", 1),  # 1
        ]

        if "stencil_valid" in root_node:
            stencil_valid = map_controller(extract_controller_json(root_node["stencil_valid"]), "stencil_valid")
            # Check actual stencil valid property of hardware before programming
            if self.stencil_valid:
                config.append(trim_config(flattened, f"loops_stencil_valid_dimensionality", stencil_valid.dim))
                config.append(trim_config(flattened, f"stencil_valid_sched_gen_sched_addr_gen_starting_addr", stencil_valid.cyc_strt))
                for i in range(stencil_valid.dim):
                    config.append(trim_config(flattened, f"loops_stencil_valid_ranges_{i}", stencil_valid.extent[i]))
                    config.append(trim_config(flattened, f"stencil_valid_sched_gen_sched_addr_gen_strides_{i}", stencil_valid.cyc_stride[i]))

        return config


if __name__ == "__main__":
    pond_dut = ValidIO(default_iterator_support=2,
                       cycle_count_width=16,
                       add_clk_enable=True,
                       add_flush=True)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    extract_formal_annotation(pond_dut, "pond.txt")

    verilog(pond_dut, filename="valid_io.sv",
            optimize_if=False)
