from kratos import *
from lake.modules.passthru import *

class LakeTop(Generator):
    def __init__(self, width):
        super().__init__("LakeTop")
        self.in_ = self.input("i_data_in", width)
        self.out_ = self.output("o_data_out", width)

        # Child instance passthru
        #passthru_child_tmp = PassThroughMod()
        #self.add_child_generator("u_passthru_0", passthru_child_tmp)
        #self.wire(self.in_[0], passthru_child_tmp.in_)
        #self.wire(self.out_[0], passthru_child_tmp.out_)

        # Add a second passthru for the other wire
        for i in range(width):
            passthru_child_tmp = PassThroughMod()
            self.add_child_generator(f"u_passthru_{i}", passthru_child_tmp)
            self.wire(self.in_[i], passthru_child_tmp.in_)
            self.wire(self.out_[i], passthru_child_tmp.out_)
