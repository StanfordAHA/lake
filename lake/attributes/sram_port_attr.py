import kratos as kts


class SRAMPortAttr(kts.Attribute):
    def __init__(self,
                 doc_string=""):
        super().__init__()
        self.value = "sram_port"
        self.documentation = doc_string

    def set_documentation(self, new_doc):
        self.documentation = new_doc

    def get_documentation(self):
        return self.documentation
