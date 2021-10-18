import kratos as kts
from lake.attributes.config_reg_attr import ConfigRegAttr


class TileBase(kts.Generator):

    def __init__(self, name: str, debug: bool):
        super().__init__(name, debug=debug)

    def add_cfg_reg(self, name, description, width, **kwargs):
        cfg_reg = self.input(name, width, **kwargs)
        cfg_reg.add_attribute(ConfigRegAttr(description))
        return cfg_reg

    # def add_tile_input(self, is_control=False, ignore=False, full_bus=False, doc_string="", ):
