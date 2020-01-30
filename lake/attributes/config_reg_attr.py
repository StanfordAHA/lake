import kratos as kts


class ConfigRegAttr(kts.Attribute):
    def __init__(self):
        super().__init__()
        self.value = "config_reg"
