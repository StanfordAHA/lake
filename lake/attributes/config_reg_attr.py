import kratos as kts


class ConfigRegAttr(kts.Attribute):

    def __init__(self,
                 doc_string=""):
        super().__init__()
        self.value = "config_reg"
        self.documentation = doc_string
        self.intercepted = False
        self.observers = []

    def set_documentation(self, new_doc):
        self.documentation = new_doc

    def get_documentation(self):
        return self.documentation

    def get_intercepted(self):
        return self.intercepted

    def set_intercepted(self, new_intercepted):
        self.intercepted = new_intercepted

    def add_observer(self, generator, port):
        self.observers.append((generator, port))

    # Get the observer variables of this config reg in
    # the generator <generator>
    def get_observers(self, generator):
        return [port for (gen, port) in self.observers if gen.internal_generator == generator]
