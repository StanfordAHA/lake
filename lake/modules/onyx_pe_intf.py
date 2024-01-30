import kratos as kts
from kratos import *
from lake.top.memory_controller import MemoryController
from lake.attributes.config_reg_attr import ConfigRegAttr
import math
from peak.assembler import Assembler
from hwtypes.modifiers import strip_modifiers
from lassen.sim import PE_fc as lassen_fc
import lassen.asm as asm
from lassen.asm import Mode_t


class OnyxPEInterface(MemoryController):
    def __init__(self,
                 data_width=16,
                 name_prefix=None,
                 include_RO_cfg=False):

        base_name = "PE"
        if name_prefix is not None:
            base_name = f"{name_prefix}{base_name}"

        self.ro_config = include_RO_cfg

        super().__init__(base_name, debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True

        # For consistency with Core wrapper in garnet...
        self.total_sets = 0

        # inputs
        self._clk = self.clock("CLK")
        # self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("ASYNCRESET")
        self._clk_en = self.clock_en("clk_en", 1)

        # Instruction
        self._inst = self.input("inst", 84)
        self._inst.add_attribute(ConfigRegAttr("PE Instruction"))

        self._data0 = self.input("data0", self.data_width)
        self._data1 = self.input("data1", self.data_width)
        self._data2 = self.input("data2", self.data_width)
        self._bit0 = self.input("bit0", 1)
        self._bit1 = self.input("bit1", 1)
        self._bit2 = self.input("bit2", 1)

        if self.ro_config:
            self._O2 = self.output("O2", self.data_width)
            self._O2.add_attribute(ConfigRegAttr("PIPE REG 0", read_only=True))
            self._O3 = self.output("O3", self.data_width)
            self._O3.add_attribute(ConfigRegAttr("PIPE REG 1", read_only=True))
            self._O4 = self.output("O4", self.data_width)
            self._O4.add_attribute(ConfigRegAttr("PIPE REG 2", read_only=True))

        # self._config_addr = self.input("config_addr", 8)
        # self._config_data = self.input("config_data", 32)
        # self._config_en = self.input("config_en", 1)

        self._O0 = self.output("O0", self.data_width)
        self._O1 = self.output("O1", 1)
        # self._O2 = self.output("O2", 2 * self.data_width)

        self.external = True

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "alu_ext"

    def get_bitstream(self, op, config_kwargs, override_dense=False, only_dense_hw=False):

        instr_type = strip_modifiers(lassen_fc.Py.input_t.field_dict['inst'])
        asm_ = Assembler(instr_type)

        kwargs = {}
        if "rb_const" in config_kwargs and config_kwargs["rb_const"] is not None:
            # the b operand is a constant
            # support constant operand for and and fp_mul for now
            assert op == 5 or op == 6
            # config the b port of pe to constant mode
            kwargs["rb_mode"] = Mode_t.CONST
            # config the value of b port constant
            kwargs["rb_const"] = config_kwargs["rb_const"]

        opcode_mapping = {
            0: asm.add(**kwargs),  # ADD
            1: asm.smult0(**kwargs),  # MUL
            2: asm.sub(**kwargs),   # SUB
            3: asm.abs(**kwargs),   # abs
            4: asm.smax(**kwargs),   # smax
            5: asm.and_(**kwargs),
            # 6: asm.fp_mul(**kwargs),
            # 7: asm.fgetfint(**kwargs),
            # 8: asm.fgetffrac(**kwargs),
            # 9: asm.faddiexp(**kwargs),
            # 10: asm.fp_max(**kwargs),
            # 11: asm.fp_add(**kwargs),
        }

        if only_dense_hw:
            op_config = op
        elif override_dense:
            print(f"OVERRIDE DENSE CONFIG: {op}")
            op_config = op
        else:
            if op not in opcode_mapping:
                raise NotImplementedError
            pe_bs = asm_.assemble(opcode_mapping[op])
            op_config = int(pe_bs)

        config_base = [("inst", op_config)]
        config = self.chop_config(config_base=config_base)

        return config


if __name__ == "__main__":

    pe_dut = OnyxPEInterface(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(pe_dut, filename="pe.sv",
            optimize_if=False)
