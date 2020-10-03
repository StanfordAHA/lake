import kratos as kts
from enum import Enum


class RangeGroupAttr(kts.Attribute):
    def __init__(self,
                 group_num,
                 group_name,
                 doc_string=""):
        super().__init__()
        self.value = "range_group"
        self.group_num = group_num
        self.group_name = group_name
        self.documentation = doc_string

    def set_documentation(self, new_doc):
        self.documentation = new_doc

    def get_documentation(self):
        return self.documentation

    def get_group_num(self):
        return self.group_num

    def geT_group_name(self):
        return self.group_name
