import kratos as kts


# this attribute indicates sram ports which may be renamed if macro being used requires
# different port names
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
