from lake.models.model import Model


class PassthruModel(Model):

    def __init__(self):
        self._val = 0

    def set_config(self, new_config):
        # Configure top level
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

    def set_in(self, in_data):
        return in_data
