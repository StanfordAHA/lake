import kratos as kts
from enum import Enum


class SharedFifoAttr(kts.Attribute):
    def __init__(self,
                 direction="IN",
                 doc_string=""):
        super().__init__()
        self.value = "control_signal"
        self.documentation = doc_string
        self.direction = direction

    def set_documentation(self, new_doc):
        self.documentation = new_doc

    def get_documentation(self):
        return self.documentation

    def set_direction(self, new_direction):
        self.direction = new_direction

    def get_direction(self):
        return self.direction
