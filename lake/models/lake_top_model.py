from lake.models.model import Model
from lake.models.agg_aligner_model import AggAlignerModel
from lake.models.agg_buff_model import AggBuffModel
from lake.models.input_addr_ctrl_model import InputAddrCtrlModel
from lake.models.output_addr_ctrl_model import OutputAddrCtrlModel
from lake.models.rw_arbiter_model import RWArbiterModel
from lake.models.sram_model import SRAMModel
from lake.models.demux_reads_model import DemuxReadsModel
from lake.models.sync_groups_model import SyncGroupsModel
from lake.models.prefetcher_model import PrefetcherModel
from lake.models.tba_model import TBAModel
import math as mt
import kratos as kts


class LakeTopModel(Model):

    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=2,
                 input_iterator_support=6,  # Addr Controllers
                 output_iterator_support=6,
                 interconnect_input_ports=1,  # Connection to int
                 interconnect_output_ports=3,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sram_stub=1,
                 agg_height=8,
                 max_agg_schedule=64,
                 input_max_port_sched=64,
                 output_max_port_sched=64,
                 align_input=1,
                 max_line_length=2048,
                 tb_height=1,
                 tb_range_max=2048,
                 tb_sched_max=64,
                 num_tb=1,
                 multiwrite=2,
                 max_prefetch=64):

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.input_iterator_support = input_iterator_support
        self.output_iterator_support = output_iterator_support
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.use_sram_stub = use_sram_stub
        self.agg_height = agg_height
        self.max_agg_schedule = max_agg_schedule
        self.input_max_port_sched = input_max_port_sched
        self.output_max_port_sched = output_max_port_sched
        self.input_port_sched_width = clog2(self.interconnect_input_ports)
        self.align_input = align_input
        self.max_line_length = max_line_length
        assert self.mem_width > self.data_width, "Data width needs to be smaller than mem"
        self.fw_int = int(self.mem_width / self.data_width)
        self.num_tb = num_tb
        self.tb_height = tb_height
        self.tb_range_max = tb_range_max
        self.tb_sched_max = tb_sched_max
        self.multiwrite = multiwrite
        self.max_prefetch = max_prefetch

        if self.banks == 1:
            self.address_width = clog2(mem_depth)
        else:
            self.address_width = clog2(mem_depth)  # + clog2(banks)

        self.config = {}

        ### INST AGG ALIGNER
        if(self.agg_height > 0):
            self.agg_aligners = []
            for i in range(self.interconnect_input_ports):
                self.agg_aligners.append(AggAlignerModel(data_width=self.data_width,
                                                         max_line_length=self.max_line_length))

        ### AGG BUFF
        self.agg_buffs = []
        for port in range(self.interconnect_input_ports):
            self.agg_buffs.append(AggBuffModel(agg_height=self.agg_height,
                                               data_width=self.data_width,
                                               mem_width=self.mem_width,
                                               max_agg_schedule=self.max_agg_schedule))

        ### INPUT ADDR CTRL
        self.iac = InputAddrCtrlModel(interconnect_input_ports=self.interconnect_input_ports,
                                      mem_depth=self.mem_depth,
                                      banks=self.banks,
                                      iterator_support=self.input_iterator_support,
                                      max_port_schedule=self.input_max_port_sched,
                                      address_width=self.address_width)

        ### OUTPUT ADDR CTRL
        self.oac = OutputAddrCtrlModel(interconnect_output_ports=self.interconnect_output_ports,
                                       mem_depth=self.mem_depth,
                                       banks=self.banks,
                                       iterator_support=self.output_iterator_support,
                                       address_width=self.address_width)

        ### RW ARBITER
        # Per bank allocation
        self.rw_arbs = []
        for bank in range(self.banks):
            self.rw_arbs.append(RWArbiterModel(fetch_width=self.mem_width,
                                               memory_depth=self.mem_depth,
                                               int_out_ports=self.interconnect_output_ports))

        ### SRAMS
        self.mems = []
        for banks in range(self.banks):
            self.mems.append(SRAMModel(width=self.mem_width,
                                       depth=self.mem_depth))

        ### DEMUX READS
        self.demux_reads = DemuxReadsModel(fetch_width=self.mem_width,
                                           banks=self.banks,
                                           int_out_ports=self.interconnect_output_ports)

        ### SYNC GROUPS
        self.sync_groups = SyncGroupsModel(fetch_width=self.mem_width,
                                           int_out_ports=self.interconnect_output_ports)

        ### PREFETCHERS
        self.prefetchers = []
        for port in range(self.interconnect_output_ports):
            self.prefetchers.append(PrefetcherModel(fetch_width=self.mem_width,
                                                    max_prefetch=self.max_prefetch))

        ###

    def set_config(self, new_config):
        # Configure top level
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val
        # Configure children
        for i in range(self.interconnect_input_ports):
            addr_gen_config = {}
            addr_gen_config["starting_addr"] = self.config[f"address_gen_{i}_starting_addr"]
            addr_gen_config["dimensionality"] = self.config[f"address_gen_{i}_dimensionality"]
            for j in range(self.iterator_support):
                addr_gen_config[f"stride_{j}"] = self.config[f"address_gen_{i}_strides_{j}"]
                addr_gen_config[f"range_{j}"] = self.config[f"address_gen_{i}_ranges_{j}"]
            self.addr_gens[i].set_config(addr_gen_config)
