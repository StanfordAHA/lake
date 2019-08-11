from kratos import *
from lake.modules.passthru import *

class LakeTop(Generator):
    def __init__(self, width):
        super().__init__("LakeTop")
        self.in_ = self.input("i_data_in", width)
        self.out_ = self.output("o_data_out", width)

        # Child instance passthru
        passthru_child_tmp = PassThroughMod()
        self.add_child_generator("u_passthru_0", passthru_child_tmp)
        self.wire(self.in_[0], passthru_child_tmp.in_)
        self.wire(self.out_[0], passthru_child_tmp.out_)

        # Add a second passthru for the other wire
        if width == 2:
            passthru_child_tmp2 = PassThroughMod()
            self.add_child_generator("u_passthru_1", passthru_child_tmp2)
            self.wire(self.in_[1], passthru_child_tmp2.in_)
            self.wire(self.out_[1], passthru_child_tmp2.out_)
