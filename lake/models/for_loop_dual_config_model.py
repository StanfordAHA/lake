from lake.models.model import Model
from kratos import *


class ForLoopDualConfigModel(Model):

    def __init__(self, iterator_support, iterator_support2, config_width):
        self.iterator_support = iterator_support
        self.iterator_support2 = iterator_support2
        self.config_width = config_width

        self.config = {}

        self.config["mux_sel_msb_init"] = 0
        self.config["dimensionality"] = 0
        self.config["dimensionality2"] = 0

        self.mux_sel = 0
        self.restart = 0
        self.dim_cnt = []

        self.mux_sel_msb = 0
        self.config_sel = 0

        for i in range(self.iterator_support):
            self.config[f"ranges_{i}"] = 0
        for i in range(self.iterator_support2):
            self.config[f"ranges2_{i}"] = 0
        for i in range(max(self.iterator_support, self.iterator_support2)):
            self.dim_cnt.append(0)
        self.mux_sel_msb = pow(2, clog2(max(self.iterator_support,
                                            self.iterator_support2)))

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

        if self.config["mux_sel_msb_init"] == 1:
            self.mux_sel = self.mux_sel_msb
            self.config_sel = 1

        if self.config_sel == 0 and self.config["dimensionality"] == 0:
            self.restart = 1
            self.config_sel = 1
        elif self.config_sel == 1 and self.config["dimensionality2"] == 0:
            self.restart = 1
            self.config_sel = 0

    def get_dim_cnt(self):
        return self.dim_cnt

    def get_mux_sel(self):
        return self.mux_sel

    def get_restart(self):
        return self.restart

    def step(self):
        # after restart, needs to clear dim_counters
        if self.restart:
            self.restart = 0
            if self.config_sel == 0:
                self.mux_sel = 0
            else:
                self.mux_sel = self.mux_sel_msb

            for i in range(max(self.iterator_support, self.iterator_support2)):
                self.dim_cnt[i] = 0

            # a corner case to skip the other config
            if self.config_sel == 0:
                if self.config["dimensionality"] == 0:
                    self.restart = 1
                    self.config_sel = 1
            else:
                if self.config["dimensionality2"] == 0:
                    self.restart = 1
                    self.config_sel = 0
        # using the first configurations
        elif (self.config_sel == 0) and (self.config["dimensionality"] != 0):
            for i in range(self.config["dimensionality"]):
                if (self.dim_cnt[i] == (self.config[f"ranges_{i}"] + 1)):
                    self.dim_cnt[i] = 0
                else:
                    self.dim_cnt[i] = self.dim_cnt[i] + 1
                    break

            # update mux_sel and restart after updating the counter
            self.mux_sel = 0
            for i in range(self.config["dimensionality"]):
                if (self.dim_cnt[i] == (self.config[f"ranges_{i}"] + 1)):
                    self.mux_sel = i + 1
                    if (i + 1) == self.config["dimensionality"]:
                        self.restart = 1
                        self.mux_sel = 0
                        self.config_sel = 1
                else:
                    break
        # same code but uses the second configurations
        else:
            for i in range(self.config["dimensionality2"]):
                if (self.dim_cnt[i] == (self.config[f"ranges2_{i}"] + 1)):
                    self.dim_cnt[i] = 0
                else:
                    self.dim_cnt[i] = self.dim_cnt[i] + 1
                    break

            self.mux_sel = self.mux_sel_msb
            for i in range(self.config["dimensionality2"]):
                if (self.dim_cnt[i] == (self.config[f"ranges2_{i}"] + 1)):
                    self.mux_sel = self.mux_sel_msb + i + 1
                    if (i + 1) == self.config["dimensionality2"]:
                        self.restart = 1
                        self.mux_sel = self.mux_sel_msb
                        self.config_sel = 0
                else:
                    break
