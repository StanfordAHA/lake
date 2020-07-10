import kratos as kts
from enum import Enum


class ControlSignalAttr(kts.Attribute):
    def __init__(self,
                 is_control=False,
                 ignore=False,
                 doc_string=""):
        super().__init__()
        self.value = "control_signal"
        self.is_control = is_control
        self.ignore = ignore
        self.documentation = doc_string

    def set_documentation(self, new_doc):
        self.documentation = new_doc

    def get_documentation(self):
        return self.documentation

    def get_control(self):
        return self.is_control

    def get_ignore(self):
        return self.ignore
