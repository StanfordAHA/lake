from abc import abstractmethod


class Model():
    def __init__(self):
        pass

    @abstractmethod
    def set_config(self, new_config):
        pass
