from lake.models.model import Model
from lake.models.unified_buffer_model import UBModel
import copy


class LakeTopModel(Model):
    '''
    Model for one MEM tile
    '''
    def __init__(self,
                 data_width=16,
                 mem_width=64,      # = fw * data_width
                 mem_depth=512,
                 fw=4,
                 agg_height=2,
                 tb_height=2,       #
                 padding_width=8,
                 delay_width=4,     #
                 interconnect_input_ports=2,
                 interconnect_output_ports=2,
                 area_opt=False):

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports

        self.padding_width = padding_width
        self.delay_width = delay_width

        self.area_opt = area_opt
        self.fw = fw
        self.agg_height = agg_height
        self.tb_height = tb_height

        # the top model only see input and output of the mem tile
        # in2agg decide WRITE which input datas into SRAM
        # tb2out decide READ which datas from SRAM
        self.agg = [UBModel(1, fw, 1, fw * agg_height), UBModel(1, fw, 1, fw * agg_height)]
        self.sram = UBModel(1, 1, fw, mem_depth)
        self.tb = [UBModel(fw, 1, 1, fw * tb_height), UBModel(fw, 1, 1, fw * tb_height)]

        self.config = {}

    def set_config(self, new_config):
        if "in2agg_1" in new_config["config"]:
            self.interconnect_input_ports = 2
            self.interconnect_output_ports = 2
        else:
            self.interconnect_input_ports = 1
            self.interconnect_output_ports = 1

        # ================================
        # original config_data might be changed by test.py
        # use deepcopy to avoid influence by modification
        # ================================
        self.config = copy.deepcopy(new_config["config"])

        # ================================
        # Check for invalid bit_width for padding and delay
        # Should already check by RTL
        # ================================
        if self.area_opt:
            for i in range(self.interconnect_input_ports):
                if "agg_read_padding" in self.config[f"agg2sram_{i}"]:
                    set_padding = self.config[f"agg2sram_{i}"]["agg_read_padding"][0]
                    max_padding = pow(2, self.padding_width)
                    if set_padding > max_padding - 1:
                        # raise Exception("agg_read_padding overflow bit_width")
                        actual_padding = set_padding % max_padding
                        self.config[f"agg2sram_{i}"]["agg_read_padding"][0] = actual_padding
                        print("\nInvalid configuration in agg_read_padding value, only support", self.padding_width, "bits usage!")
                        print("===== Trim to", actual_padding, "\n")

                if "delay" in self.config[f"agg2sram_{i}"]:
                    set_delay = self.config[f"agg2sram_{i}"]["delay"][0]
                    max_delay = pow(2, self.delay_width)
                    if set_delay > max_delay - 1:
                        # raise Exception("delay overflow bit_width")
                        actual_delay = set_delay % max_delay
                        self.config[f"agg2sram_{i}"]["delay"][0] = actual_delay
                        print("\n Invalid configuration in delay value, only support", self.delay_width, "bits usage!")
                        print("===== Trim to", actual_delay, "\n")
        return

    def controller(self, module_name, starting_addr, stride, max_addr=None):
        # ================================
        # check dim consistency, should be done by config generator
        # ================================
        dim = int(self.config[module_name]["dimensionality"])
        if dim != len(stride):
            print("\n dimensionality is inconsistent with dim of stride in", module_name)
            print(module_name, "dim should be", len(stride))
            raise Exception("dim is inconsistent with dim of stride")
            dim = len(stride)

        # =============
        # affince access pattern
        # =============
        extent = self.config[module_name]["extent"]
        start_addr = starting_addr[0]
        step = []
        for i in range(dim):
            step.append(1)

        sched = []
        while True:
            next_addr = start_addr
            for i in range(dim):
                next_addr += stride[i] * (step[i] - 1)
            if max_addr is not None:
                next_addr %= max_addr
            sched.append(next_addr)
            if step == extent:
                break
            for i in range(dim):
                if step[i] == extent[i]:
                    step[i] = 1
                    continue
                step[i] += 1
                break
        return sched

    # ================
    # area_opt = True, padding mode
    # ================
    def agg2sram_padding_sg(self, in2agg_extent, in2agg_ag, in2agg_sg, padding):
        sched = []
        for i in range(len(in2agg_ag)):
            if in2agg_ag[i] % 4 == 3:     # potential bug need to be fix
                sched.append(in2agg_sg[i] + 1)
            if padding != 0 and (i + 1) % in2agg_extent == 0:
                sched.append(in2agg_sg[i] + padding)
        sched.sort()
        return sched

    def agg2sram_linear_ag(self, agg2sram_sg, starting_addr, max_addr=None):
        sched = []
        current_addr = starting_addr
        for i in range(len(agg2sram_sg)):
            sched.append(current_addr)
            current_addr += 1
            if max_addr is not None:
                current_addr %= max_addr
        return sched

    # ================
    # area_opt = True, delay mode
    # ================
    def agg2sram_delay_sg(self, sram2tb_sg, delay):
        sched = []
        for i in range(len(sram2tb_sg)):
            sched.append(sram2tb_sg[i] + delay + 1)
        return sched

    def in2agg_delay_sg(self, tb2out_sg, in2agg_config, tb2out_config):
        starting_sched = in2agg_config["cycle_starting_addr"][0]
        delay = starting_sched - tb2out_config["cycle_starting_addr"][0]
        stride = in2agg_config["cycle_stride"]
        extent = in2agg_config["extent"]
        dim = len(stride)
        max_addr = self.fw * self.agg_height

        new_stride = [stride[0]]
        for i in range(1, len(stride)):
            diff = stride[i]
            for j in range(i, 0, -1):
                diff -= stride[j - 1] * (extent[j - 1] - 1)
            new_stride.append(diff)
        tb2out_sg_d = [sg + delay for sg in tb2out_sg]

        addr = []
        current_addr = in2agg_config["write_data_starting_addr"][0]
        if dim == 1:
            for j in range(len(tb2out_sg_d)):
                addr.append(current_addr)
                current_addr += 1
                if (j % (extent[0] - 1) == 0):     # fix to start at 0 or 1
                    if 0 < current_addr <= max_addr // 2:
                        current_addr = max_addr // 2
                    else:
                        current_addr = 0
            return tb2out_sg_d, addr

        sched = []
        step = []
        for i in range(dim):
            step.append(1)
        mux_sel = 1
        step2 = extent[0] - 1
        current_sched = starting_sched
        flag = 0
        while True:
            if step2 > len(tb2out_sg_d) - 1:
                break
            if (step2) % tb2out_config["extent"][0] == 0:
                ## mul_sel is set to 1, but not yet clear
                this_dim_end = tb2out_sg_d[step2 - 1] + stride[0]
                # print(current_sched, this_dim_end)
            else:
                this_dim_end = tb2out_sg_d[step2] - tb2out_config["cycle_stride"][0] + stride[0]
                # print(current_sched, this_dim_end)
            end_sched = current_sched
            for j in range(current_sched, this_dim_end + 1, stride[0]):
                sched.append(j)
                addr.append(current_addr)
                current_addr += 1
                if step[0] == extent[0]:     # fix to start at 0 or 1
                    if 0 < current_addr <= max_addr // 2:
                        current_addr = max_addr // 2
                    else:
                        current_addr = 0
                    step[0] = 1
                else:
                    step[0] += 1
                end_sched = j

            for i in range(1, dim):
                if step[i] == extent[i]:
                    step[i] = 1
                    mux_sel = i + 1
                    continue
                step[i] += 1
                mux_sel = i
                break

            if mux_sel > dim - 1:
                break
            # print(new_stride, mux_sel)
            current_sched = end_sched + new_stride[mux_sel]
            step2 += extent[0]
        return sched, addr

    # ==============
    # call by test.py
    # generate golden_data_out
    # ==============
    def interact(self, data_in_list):

        # =======================
        # Controller list for two port
        # =======================
        agg_write_sg_list = []
        agg_write_ag_list = []
        agg_read_sg_list = []
        agg_read_ag_list = []
        sram_write_ag_list = []
        sram_read_sg_list = []
        sram_read_ag_list = []
        tb_write_ag_list = []
        tb_read_sg_list = []
        tb_read_ag_list = []

        in2agg_step = []
        agg2sram_step = []
        sram2tb_step = []
        tb2out_step = []

        data_out_list = []
        valid_out_list = []
        data_in_sched_list = []

        sram_rd_reg = []
        stop = []

        for i in range(self.interconnect_input_ports):
            # ========= sram2tb ===========
            sram_read_sg = self.controller(f"sram2tb_{i}", self.config[f"sram2tb_{i}"]["cycle_starting_addr"], self.config[f"sram2tb_{i}"]["cycle_stride"])
            sram_read_sg_list.append(sram_read_sg)
            sram_read_ag = self.controller(f"sram2tb_{i}", self.config[f"sram2tb_{i}"]["read_data_starting_addr"], self.config[f"sram2tb_{i}"]["read_data_stride"], max_addr=self.mem_depth)
            sram_read_ag_list.append(sram_read_ag)
            # print(sram_read_ag)
            tb_write_ag = self.controller(f"sram2tb_{i}", self.config[f"sram2tb_{i}"]["write_data_starting_addr"], self.config[f"sram2tb_{i}"]["write_data_stride"], max_addr=self.fw * self.tb_height)
            tb_write_ag_list.append(tb_write_ag)

            # ========= tb2out ===========
            tb_read_sg = self.controller(f"tb2out_{i}", self.config[f"tb2out_{i}"]["cycle_starting_addr"], self.config[f"tb2out_{i}"]["cycle_stride"])
            tb_read_sg_list.append(tb_read_sg)
            tb_read_ag = self.controller(f"tb2out_{i}", self.config[f"tb2out_{i}"]["read_data_starting_addr"], self.config[f"tb2out_{i}"]["read_data_stride"], max_addr=self.fw * self.tb_height)
            tb_read_ag_list.append(tb_read_ag)

        for i in range(self.interconnect_input_ports):
            # ========= in2agg ===========
            if self.area_opt:
                mode = self.config[f"agg2sram_{i}"]["mode"][0]
                if mode == 0:
                    agg_write_sg = self.controller(f"in2agg_{i}", self.config[f"in2agg_{i}"]["cycle_starting_addr"], self.config[f"in2agg_{i}"]["cycle_stride"])
                    agg_write_ag = self.controller(f"in2agg_{i}", self.config[f"in2agg_{i}"]["write_data_starting_addr"], self.config[f"in2agg_{i}"]["write_data_stride"], max_addr=self.fw * self.agg_height)

                elif mode == 2:
                    agg_write_sg, agg_write_ag = self.in2agg_delay_sg(tb_read_sg_list[0], self.config[f"in2agg_{i}"], self.config[f"tb2out_0"])
                else:
                    agg_write_sg, agg_write_ag = self.in2agg_delay_sg(tb_read_sg_list[1], self.config[f"in2agg_{i}"], self.config[f"tb2out_1"])
            else:
                agg_write_sg = self.controller(f"in2agg_{i}", self.config[f"in2agg_{i}"]["cycle_starting_addr"], self.config[f"in2agg_{i}"]["cycle_stride"])
                agg_write_ag = self.controller(f"in2agg_{i}", self.config[f"in2agg_{i}"]["write_data_starting_addr"], self.config[f"in2agg_{i}"]["write_data_stride"], max_addr=self.fw * self.agg_height)
            # print(agg_write_sg)
            # print(agg_write_ag)
            agg_write_sg_list.append(agg_write_sg)
            agg_write_ag_list.append(agg_write_ag)

        for i in range(self.interconnect_input_ports):
            # ========= agg2sram ============
            if self.area_opt:
                mode = self.config[f"agg2sram_{i}"]["mode"][0]
                if mode == 0:
                    agg_read_sg = self.agg2sram_padding_sg(self.config[f"in2agg_{i}"]["extent"][0], agg_write_ag_list[i], agg_write_sg_list[i], self.config[f"agg2sram_{i}"]["agg_read_padding"][0])
                    sram_write_ag = self.agg2sram_linear_ag(agg_read_sg, self.config[f"agg2sram_{i}"]["write_data_starting_addr"][0], max_addr=self.mem_depth)
                    agg_read_ag = [addr % (self.fw * self.agg_height) for addr in sram_write_ag]
                elif mode == 2:
                    agg_read_sg = self.agg2sram_delay_sg(sram_read_sg_list[0], self.config[f"agg2sram_{i}"]["delay"][0])
                    sram_write_ag = sram_read_ag_list[0]
                    agg_read_ag = [addr % (self.fw * self.agg_height) for addr in sram_write_ag]
                else:
                    agg_read_sg = self.agg2sram_delay_sg(sram_read_sg_list[1], self.config[f"agg2sram_{i}"]["delay"][0])
                    sram_write_ag = sram_read_ag_list[1]
                    agg_read_ag = [addr % (self.fw * self.agg_height) for addr in sram_write_ag]
            else:
                agg_read_sg = self.controller(f"agg2sram_{i}", self.config[f"agg2sram_{i}"]["cycle_starting_addr"], self.config[f"agg2sram_{i}"]["cycle_stride"])
                agg_read_ag = self.controller(f"agg2sram_{i}", self.config[f"agg2sram_{i}"]["read_data_starting_addr"], self.config[f"agg2sram_{i}"]["read_data_stride"], max_addr=self.fw * self.agg_height)
                sram_write_ag = self.controller(f"agg2sram_{i}", self.config[f"agg2sram_{i}"]["write_data_starting_addr"], self.config[f"agg2sram_{i}"]["write_data_stride"], max_addr=self.mem_depth)
            agg_read_sg_list.append(agg_read_sg)
            agg_read_ag_list.append(agg_read_ag)
            sram_write_ag_list.append(sram_write_ag)

        # ===============
        # check invalid single port sched
        # should be done by config generator
        # ===============
        uniq = set()
        for i in range(self.interconnect_input_ports):
            for sg in [agg_read_sg_list, sram_read_sg_list]:
                if (set(sg[i]) & uniq):
                    print("========== Invalid single port sched =============")
                    return None, None, None
                else:
                    uniq.update(set(sg[i]))

        # ===============
        # initialization for simulation
        # ===============
        for i in range(self.interconnect_input_ports):
            in2agg_step.append(0)
            agg2sram_step.append(0)
            sram2tb_step.append(0)
            tb2out_step.append(0)

            data_out_list.append([])
            valid_out_list.append([])
            data_in_sched_list.append([])

            sram_rd_reg.append(0)
            stop.append(0)

        cycle_count = 0
        in_list = []
        agg_list = []
        sram_list = []
        tb_list = []
        while True:
            # ================
            # sram write for each port
            # ================
            for i in range(self.interconnect_input_ports):
                # first read then write
                if agg2sram_step[i] < len(agg_read_sg_list[i]):
                    if cycle_count == agg_read_sg_list[i][agg2sram_step[i]]:
                        agg_rd_reg = self.agg[i].interact(0, 1, agg_read_ag_list[i][agg2sram_step[i]], 0)
                        agg_list.append([cycle_count, agg_rd_reg])
                        self.sram.interact(1, 0, sram_write_ag_list[i][agg2sram_step[i]], agg_rd_reg)
                        # print(cycle_count, sram_write_ag_list[i][agg2sram_step[i]], agg_rd_reg)
                        agg2sram_step[i] += 1

                if in2agg_step[i] < len(agg_write_sg_list[i]):
                    if cycle_count == agg_write_sg_list[i][in2agg_step[i]]:
                        if cycle_count < len(data_in_list[i]):
                            self.agg[i].interact(1, 0, agg_write_ag_list[i][in2agg_step[i]], data_in_list[i][agg_write_sg_list[i][in2agg_step[i]]])
                            in_list.append([cycle_count, data_in_list[i][agg_write_sg_list[i][in2agg_step[i]]]])
                            # print(agg_write_ag[in2agg_step[i]], data_in_list[i][agg_write_sg_list[i][in2agg_step[i]]])
                        else:
                            self.agg[i].interact(1, 0, agg_write_ag_list[i][in2agg_step[i]], 0)
                        in2agg_step[i] += 1
                        data_in_sched_list[i].append(1)
                    else:
                        data_in_sched_list[i].append(0)

            # ================
            # sram read for each port
            # ================
            for i in range(self.interconnect_input_ports):
                # tb rw same cycle = false
                # first read then write
                if tb2out_step[i] < len(tb_read_sg_list[i]):
                    if cycle_count == tb_read_sg_list[i][tb2out_step[i]]:
                        tb_rd_reg = self.tb[i].interact(0, 1, tb_read_ag_list[i][tb2out_step[i]], 0)
                        # print([cycle_count, tb_read_ag_list[i][tb2out_step[i]], tb_rd_reg])
                        tb_list.append([cycle_count, tb_rd_reg])
                        tb2out_step[i] += 1
                        valid_out_list[i].append(1)
                    else:
                        tb_rd_reg = 0
                        valid_out_list[i].append(0)
                    data_out_list[i].append(tb_rd_reg)
                else:
                    valid_out_list[i].append(0)
                    stop[i] = 1
                    # finish output of interest (read tb), no need to do the write
                    continue

                # ================
                # tb write delay one cycle from sram read
                # ================
                if sram2tb_step[i] < len(sram_read_sg_list[i]):
                    if cycle_count == sram_read_sg_list[i][sram2tb_step[i]] + 1:
                        self.tb[i].interact(1, 0, tb_write_ag_list[i][sram2tb_step[i]], sram_rd_reg[i])
                        # print([cycle_count, tb_write_ag_list[i][sram2tb_step[i]]%2*self.fw, sram_rd_reg[i]])
                        sram2tb_step[i] += 1
                    if (sram2tb_step[i] < len(sram_read_sg_list[i])) and (cycle_count == sram_read_sg_list[i][sram2tb_step[i]]):
                        sram_rd_reg[i] = self.sram.interact(0, 1, sram_read_ag_list[i][sram2tb_step[i]], 0)
                        sram_list.append([cycle_count + 1, sram_read_ag_list[i][sram2tb_step[i]], sram_rd_reg[i]])

            # ================
            # stop if both port finish
            # ================
            if all(stop):
                break
            cycle_count += 1

        # ================
        # for debug
        # ================
        # print(in_list)
        # print(agg_list)
        # print(sram_list)
        # print(tb_list)
        return data_out_list, valid_out_list, data_in_sched_list
