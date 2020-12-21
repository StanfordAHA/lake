import kratos as kts
from enum import Enum
from enum import auto


class FormalSignalConstraint(Enum):
    X = auto()
    SET0 = auto()
    SET1 = auto()
    CLK = auto()
    RSTN = auto()
    SOLVE = auto()
    SEQUENCE = auto()


# parent class for formal attributes
class FormalAttrBase(kts.Attribute):
    def __init__(self,
                 port_name,
                 formalsig_cnst,
                 doc_string=""):
        super().__init__()
        self.port_name = port_name
        self.formalsig_cnst = formalsig_cnst
        self.documentation = doc_string
        self.annotation = f"{self.port_name}\t{self.formalsig_cnst.name}"

    def set_documentation(self, new_doc):
        self.documentation = new_doc

    def get_documentation(self):
        return self.documentation

    def get_annotation(self):
        return self.annotation

    def get_port_name(self):
        return self.port_name

    def get_formal_ann(self):
        return self.formalsig_cnst

# these attributes should be used for all modules (agg, sram, tb)
class FormalAttr(FormalAttrBase):
    def __init__(self,
                 port_name,
                 formalsig_cnst,
                 doc_string=""):
        super().__init__(port_name, formalsig_cnst, doc_string)

# these are agg specific formal attributes
class AggFormalAttr(FormalAttrBase):
    def __init__(self,
                 port_name,
                 formalsig_cnst,
                 doc_string=""):
        super().__init__(port_name, formalsig_cnst, doc_string)

# these are sram specific formal attributes
class SRAMFormalAttr(FormalAttrBase):
    def __init__(self,
                 port_name,
                 formalsig_cnst,
                 doc_string=""):
        super().__init__(port_name, formalsig_cnst, doc_string)

# these are tb specific formal attributes
class TBFormalAttr(FormalAttrBase):
    def __init__(self,
                 port_name,
                 formalsig_cnst,
                 doc_string=""):
        super().__init__(port_name, formalsig_cnst, doc_string)
