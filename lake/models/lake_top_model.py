from lake.models.model import Model
from lake.models.agg_aligner_model import AggAlignerModel
from lake.models.agg_buff_model import AggBuffModel
from lake.models.input_addr_ctrl_model import InputAddrCtrlModel
from lake.models.output_addr_ctrl_model import OutputAddrCtrlModel
from lake.models.rw_arbiter_model import RWArbiterModel
from lake.models.demux_reads_model import DemuxReadsModel
from lake.models.sync_groups_model import SyncGroupsModel
from lake.models.prefetcher_model import PrefetcherModel
from lake.models.tba_model import TBAModel
from lake.models.register_file_model import RegisterFileModel
from lake.models.app_ctrl_model import AppCtrlModel
from lake.models.sram_wrapper_model import SRAMWrapperModel
# from lake.models.chain_model import ChainModel
from typing import List
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
                 sram_name="default_name",
                 read_delay=1,
                 agg_height=8,
                 max_agg_schedule=64,
                 input_max_port_sched=64,
                 output_max_port_sched=64,
                 align_input=1,
                 max_line_length=2048,
                 tb_height=1,
                 tb_range_max=2048,
                 tb_range_inner_max=5,
                 tb_sched_max=64,
                 num_tb=1,
                 multiwrite=2,
                 max_prefetch=64,
                 num_tiles=1,
                 stcl_valid_iter=4):

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
        self.sram_name = sram_name
        self.agg_height = agg_height
        self.max_agg_schedule = max_agg_schedule
        self.input_max_port_sched = input_max_port_sched
        self.output_max_port_sched = output_max_port_sched
        self.input_port_sched_width = kts.clog2(self.interconnect_input_ports)
        self.align_input = align_input
        self.max_line_length = max_line_length
        assert self.mem_width > self.data_width, "Data width needs to be smaller than mem"
        self.fw_int = int(self.mem_width / self.data_width)
        self.num_tb = num_tb
        self.tb_height = tb_height
        self.tb_range_max = tb_range_max
        self.tb_range_inner_max = tb_range_inner_max
        self.tb_sched_max = tb_sched_max
        self.multiwrite = multiwrite
        self.max_prefetch = max_prefetch
        self.read_delay = read_delay
        self.num_tiles = num_tiles
        self.stcl_valid_iter = stcl_valid_iter

        self.chain_idx_bits = max(1, kts.clog2(num_tiles))
        self.address_width = kts.clog2(self.mem_depth * num_tiles)

        self.config = {}

        # top level configuration registers

        # chaining
        self.config[f"enable_chain_input"] = 0
        self.config[f"enable_chain_output"] = 0
        self.config[f"chain_idx_input"] = 0
        self.config[f"chain_idx_output"] = 0

        self.config[f"tile_en"] = 0
        self.config[f"mode"] = 0
        for i in range(self.interconnect_output_ports):
            self.config[f"rate_matched_{i}"] = 0

        # Set up model..
        ### APP CTRL (FINE-GRAINED)
        self.app_ctrl = AppCtrlModel(int_in_ports=self.interconnect_input_ports,
                                     int_out_ports=self.interconnect_output_ports,
                                     sprt_stcl_valid=True,
                                     stcl_iter_support=self.stcl_valid_iter)
        for i in range(self.interconnect_input_ports):
            self.config[f"app_ctrl_write_depth_{i}"] = 0
        for i in range(self.interconnect_output_ports):
            self.config[f"app_ctrl_input_port_{i}"] = 0
            self.config[f"app_ctrl_read_depth_{i}"] = 0
            self.config[f"app_ctrl_prefill_{i}"] = 0

        # calculate stencil_valid with fine-grained application controller
        for i in range(self.stcl_valid_iter):
            self.config[f'app_ctrl_ranges_{i}'] = 0
            self.config[f'app_ctrl_app_ctrl_threshold_{i}'] = 0

        ### COARSE APP CTRL
        self.app_ctrl_coarse = AppCtrlModel(int_in_ports=self.interconnect_input_ports,
                                            int_out_ports=self.interconnect_output_ports,
                                            sprt_stcl_valid=False,
                                            # unused, stencil valid comes from app ctrl,
                                            # not coarse app ctrl
                                            stcl_iter_support=0)

        for i in range(self.interconnect_input_ports):
            self.config[f"app_ctrl_coarse_write_depth_{i}"] = 0
        for i in range(self.interconnect_output_ports):
            self.config[f"app_ctrl_coarse_input_port_{i}"] = 0
            self.config[f"app_ctrl_coarse_read_depth_{i}"] = 0
            self.config[f"app_ctrl_coarse_prefill_{i}"] = 0

        ### INST AGG ALIGNER
        if(self.agg_height > 0):
            self.agg_aligners = []
            for i in range(self.interconnect_input_ports):
                self.agg_aligners.append(AggAlignerModel(data_width=self.data_width,
                                                         max_line_length=self.max_line_length))
                self.config[f"agg_align_{i}_line_length"] = 0

        ### AGG BUFF
        self.agg_buffs = []
        for port in range(self.interconnect_input_ports):
            self.agg_buffs.append(AggBuffModel(agg_height=self.agg_height,
                                               data_width=self.data_width,
                                               mem_width=self.mem_width,
                                               max_agg_schedule=self.max_agg_schedule))

            self.config[f"agg_in_{i}_in_period"] = 0
            self.config[f"agg_in_{i}_out_period"] = 0
            for j in range(self.max_agg_schedule):
                self.config[f"agg_in_{i}_in_sched_{j}"] = 0
                self.config[f"agg_in_{i}_out_sched_{j}"] = 0

        ### INPUT ADDR CTRL
        self.iac = InputAddrCtrlModel(interconnect_input_ports=self.interconnect_input_ports,
                                      data_width=self.data_width,
                                      fetch_width=self.mem_width,
                                      mem_depth=self.mem_depth,
                                      num_tiles=self.num_tiles,
                                      banks=self.banks,
                                      iterator_support=self.input_iterator_support,
                                      max_port_schedule=self.input_max_port_sched,
                                      address_width=self.address_width)

        for i in range(self.interconnect_input_ports):
            self.config[f"input_addr_ctrl_address_gen_{i}_dimensionality"] = 0
            self.config[f"input_addr_ctrl_address_gen_{i}_starting_addr"] = 0
            for j in range(self.input_iterator_support):
                self.config[f"input_addr_ctrl_address_gen_{i}_ranges_{j}"] = 0
                self.config[f"input_addr_ctrl_address_gen_{i}_strides_{j}"] = 0
            for j in range(self.multiwrite):
                self.config[f"input_addr_ctrl_offsets_cfg_{i}_{j}"] = 0

        ### OUTPUT ADDR CTRL
        self.oac = OutputAddrCtrlModel(interconnect_output_ports=self.interconnect_output_ports,
                                       mem_depth=self.mem_depth,
                                       num_tiles=self.num_tiles,
                                       data_width=self.data_width,
                                       fetch_width=self.mem_width,
                                       banks=self.banks,
                                       iterator_support=self.output_iterator_support,
                                       address_width=self.address_width,
                                       chain_idx_output=self.config[f"chain_idx_output"])
        for i in range(self.interconnect_output_ports):
            self.config[f"output_addr_ctrl_address_gen_{i}_dimensionality"] = 0
            self.config[f"output_addr_ctrl_address_gen_{i}_starting_addr"] = 0
            for j in range(self.input_iterator_support):
                self.config[f"output_addr_ctrl_address_gen_{i}_ranges_{j}"] = 0
                self.config[f"output_addr_ctrl_address_gen_{i}_strides_{j}"] = 0

        ### RW ARBITER
        # Per bank allocation
        self.rw_arbs = []
        for bank in range(self.banks):
            self.rw_arbs.append(RWArbiterModel(fetch_width=self.mem_width,
                                               data_width=self.data_width,
                                               memory_depth=self.mem_depth,
                                               int_out_ports=self.interconnect_output_ports,
                                               read_delay=self.read_delay))

        self.mems = []
        if self.read_delay == 1:
            ### SRAMS
            for banks in range(self.banks):
                self.mems.append(SRAMWrapperModel(use_sram_stub=self.use_sram_stub,
                                                  sram_name=self.sram_name,
                                                  data_width=self.data_width,
                                                  fw_int=self.fw_int,
                                                  mem_depth=self.mem_depth,
                                                  mem_input_ports=self.mem_input_ports,
                                                  mem_output_ports=self.mem_output_ports,
                                                  address_width=self.address_width,
                                                  bank_num=banks,
                                                  num_tiles=self.num_tiles,
                                                  enable_chain_input=self.config[f"enable_chain_input"],
                                                  enable_chain_output=self.config[f"enable_chain_output"],
                                                  chain_idx_input=self.config[f"chain_idx_input"],
                                                  chain_idx_output=self.config[f"chain_idx_output"]))
        else:
            ### REGFILES
            for banks in range(self.banks):
                self.mems.append(RegisterFileModel(data_width=self.data_width,
                                                   write_ports=self.mem_input_ports,
                                                   read_ports=self.mem_output_ports,
                                                   width_mult=self.fw_int,
                                                   depth=self.mem_depth))

        ### DEMUX READS
        self.demux_reads = DemuxReadsModel(fetch_width=self.mem_width,
                                           data_width=self.data_width,
                                           banks=self.banks,
                                           int_out_ports=self.interconnect_output_ports)

        ### SYNC GROUPS
        self.sync_groups = SyncGroupsModel(fetch_width=self.mem_width,
                                           data_width=self.data_width,
                                           int_out_ports=self.interconnect_output_ports)
        for i in range(self.interconnect_output_ports):
            self.config[f"sync_grp_sync_group_{i}"] = 0

        ### PREFETCHERS
        self.prefetchers = []
        for port in range(self.interconnect_output_ports):
            self.prefetchers.append(PrefetcherModel(fetch_width=self.mem_width,
                                                    data_width=self.data_width,
                                                    max_prefetch=self.max_prefetch))
            self.config[f"pre_fetch_{port}_input_latency"] = 0

        ### TBAS
        self.tbas = []
        for port in range(self.interconnect_output_ports):
            self.tbas.append(TBAModel(word_width=self.data_width,
                                      fetch_width=self.fw_int,
                                      num_tb=self.num_tb,
                                      tb_height=self.tb_height,
                                      max_range=self.tb_range_max,
                                      max_range_inner=self.tb_range_inner_max))
            for i in range(self.tb_height):
                self.config[f"tba_{port}_tb_{i}_range_inner"] = 0
                self.config[f"tba_{port}_tb_{i}_range_outer"] = 0
                self.config[f"tba_{port}_tb_{i}_stride"] = 0
                self.config[f"tba_{port}_tb_{i}_dimensionality"] = 0
                self.config[f"tba_{port}_tb_{i}_starting_addr"] = 0
                for j in range(self.tb_sched_max):
                    self.config[f"tba_{port}_tb_{i}_indices_{j}"] = 0

        ### CHAINING
        # self.chain = ChainModel(data_width=self.data_width,
        #                         interconnect_output_ports=self.interconnect_output_ports,
        #                         chain_idx_bits=self.chain_idx_bits,
        #                         enable_chain_output=self.config[f"enable_chain_output"],
        #                         chain_idx_output=self.config[f"chain_idx_output"])
        # chaining configuration registers are passed down from top level

    def set_config(self, new_config):
        # Configure top level
        for key, config_val in new_config.items():
            edited_key = key
            # Hack for now to not need to reconfigure everything
            if "strg_ub_" in key:
                edited_key = key[8:]
            assert edited_key in self.config, f"{edited_key} is not in the configuration..."
            self.config[edited_key] = config_val

        # Configure children

        # Config App Ctrl
        app_ctrl_cfg = {}
        for i in range(self.interconnect_input_ports):
            app_ctrl_cfg[f"write_depth_{i}"] = self.config[f"app_ctrl_write_depth_{i}"]
        for i in range(self.interconnect_output_ports):
            app_ctrl_cfg[f"read_depth_{i}"] = self.config[f"app_ctrl_read_depth_{i}"]
            app_ctrl_cfg[f"input_port_{i}"] = self.config[f"app_ctrl_input_port_{i}"]
            app_ctrl_cfg[f"prefill_{i}"] = self.config[f"app_ctrl_prefill_{i}"]

        for i in range(self.stcl_valid_iter):
            app_ctrl_cfg[f"ranges_{i}"] = self.config[f'app_ctrl_ranges_{i}']
            app_ctrl_cfg[f"threshold_{i}"] = self.config[f'app_ctrl_app_ctrl_threshold_{i}']
        self.app_ctrl.set_config(app_ctrl_cfg)

        # Config Coarse App Ctrl
        app_ctrl_coarse_cfg = {}
        for i in range(self.interconnect_input_ports):
            app_ctrl_coarse_cfg[f"write_depth_{i}"] = self.config[f"app_ctrl_coarse_write_depth_{i}"]
        for i in range(self.interconnect_output_ports):
            app_ctrl_coarse_cfg[f"read_depth_{i}"] = self.config[f"app_ctrl_coarse_read_depth_{i}"]
            app_ctrl_coarse_cfg[f"input_port_{i}"] = self.config[f"app_ctrl_coarse_input_port_{i}"]
            app_ctrl_coarse_cfg[f"prefill_{i}"] = self.config[f"app_ctrl_coarse_prefill_{i}"]
        self.app_ctrl_coarse.set_config(app_ctrl_coarse_cfg)

        # Config Agg Align
        for i in range(self.interconnect_input_ports):
            agg_aligner_config = {}
            agg_aligner_config['line_length'] = self.config[f"agg_align_{i}_line_length"]
            self.agg_aligners[i].set_config(agg_aligner_config)

        # Config Agg Buff
        for i in range(self.interconnect_input_ports):
            agg_buff_config = {}
            agg_buff_config['in_period'] = self.config[f"agg_in_{i}_in_period"]
            agg_buff_config['out_period'] = self.config[f"agg_in_{i}_out_period"]
            for j in range(self.max_agg_schedule):
                agg_buff_config[f"in_sched_{j}"] = self.config[f"agg_in_{i}_in_sched_{j}"]
                agg_buff_config[f"out_sched_{j}"] = self.config[f"agg_in_{i}_out_sched_{j}"]
            self.agg_buffs[i].set_config(agg_buff_config)

        # Config IAC
        iac_config = {}
        for i in range(self.interconnect_input_ports):
            iac_config[f"address_gen_{i}_starting_addr"] = \
                self.config[f"input_addr_ctrl_address_gen_{i}_starting_addr"]
            iac_config[f"address_gen_{i}_dimensionality"] = \
                self.config[f"input_addr_ctrl_address_gen_{i}_dimensionality"]
            for j in range(self.input_iterator_support):
                iac_config[f"address_gen_{i}_ranges_{j}"] = \
                    self.config[f"input_addr_ctrl_address_gen_{i}_ranges_{j}"]
                iac_config[f"address_gen_{i}_strides_{j}"] = \
                    self.config[f"input_addr_ctrl_address_gen_{i}_strides_{j}"]
        self.iac.set_config(iac_config)

        # Config OAC
        oac_config = {}
        for i in range(self.interconnect_output_ports):
            oac_config[f"address_gen_{i}_starting_addr"] = \
                self.config[f"output_addr_ctrl_address_gen_{i}_starting_addr"]
            oac_config[f"address_gen_{i}_dimensionality"] = \
                self.config[f"output_addr_ctrl_address_gen_{i}_dimensionality"]
            for j in range(self.output_iterator_support):
                oac_config[f"address_gen_{i}_ranges_{j}"] = \
                    self.config[f"output_addr_ctrl_address_gen_{i}_ranges_{j}"]
                oac_config[f"address_gen_{i}_strides_{j}"] = \
                    self.config[f"output_addr_ctrl_address_gen_{i}_strides_{j}"]
        self.oac.set_config(oac_config)

        # Config RWArbiter
        rw_arb_config = {}
        for i in range(self.banks):
            self.rw_arbs[i].set_config(rw_arb_config)

        # Config SRAM
        sram_config = {}
        for i in range(self.banks):
            self.mems[i].set_config(sram_config)

        # Config DEMUX Reads
        demux_reads_config = {}
        self.demux_reads.set_config(demux_reads_config)

        # Config Sync Groups
        sync_groups_config = {}
        for i in range(self.interconnect_output_ports):
            sync_groups_config[f'sync_group_{i}'] = self.config[f"sync_grp_sync_group_{i}"]
        self.sync_groups.set_config(sync_groups_config)

        # Config Prefetcher
        for i in range(self.interconnect_output_ports):
            prefetch_config = {}
            prefetch_config['input_latency'] = self.config[f"pre_fetch_{i}_input_latency"]
            self.prefetchers[i].set_config(prefetch_config)

        # Config TBA
        for port in range(self.interconnect_output_ports):
            tba_config = {}
            # for i in range(self.tb_height):
            tba_config[f"range_inner"] = self.config[f"tba_{port}_tb_0_range_inner"]
            tba_config[f"range_outer"] = self.config[f"tba_{port}_tb_0_range_outer"]
            tba_config[f"stride"] = self.config[f"tba_{port}_tb_0_stride"]
            tba_config["indices"] = []
            tba_config[f"dimensionality"] = self.config[f"tba_{port}_tb_0_dimensionality"]
            tba_config[f"starting_addr"] = self.config[f"tba_{port}_tb_0_starting_addr"]
            for j in range(self.tb_sched_max):
                tba_config[f"indices"].append(self.config[f"tba_{port}_tb_0_indices_{j}"])
            self.tbas[port].set_config(tba_config)

        self.tb_valid = [0] * self.interconnect_output_ports

        self.mem_data_out = []
        self.mem_valid_data = []
        for i in range(self.banks):
            self.mem_data_out.append(0)
            self.mem_valid_data.append(0)

        self.iac_port_out = []
        self.valid_agg_buff = []
        for i in range(self.interconnect_input_ports):
            self.iac_port_out.append(0)
            self.valid_agg_buff.append(0)

    def interact(self,
                 chain_valid_in,
                 chain_data_in,
                 data_in,
                 addr_in,
                 wen,
                 ren):
        '''
        Top level interactions - - -
        returns (chain_data_out, chain_valid_out, data_out, valid_out)
        '''

        if self.config[f"tile_en"] == 0:
            emptyout = [0] * self.interconnect_output_ports
            return emptyout, emptyout, emptyout, emptyout

        (ac_wen_out,
         ac_ren_out,
         ac_valid_out_dat,
         ac_valid_out_stencil) = self.app_ctrl.interact(wen_in=wen,
                                                        ren_in=ren,
                                                        tb_valid=self.tb_valid,
                                                        ren_update=self.tb_valid)

        (arb_wen_en,
         arb_ren_en,
         acc_valid_out_dat,
         acc_valid_out_stencil) = self.app_ctrl_coarse.interact(wen_in=self.iac_port_out & self.valid_agg_buff,
                                                                ren_in=1,
                                                                tb_valid=0,
                                                                ren_update=ack_reduced)

        if self.align_input:
            valids_align = []
            aligns_align = []
            data_align = []
            # Pass data through agg aligners
            for i in range(self.interconnect_input_ports):
                (d_out, v_out, a_out) = self.agg_aligners[i].interact(data_in[i],
                                                                      ac_wen_out[i])
                data_align.append(d_out)
                valids_align.append(v_out)
                aligns_align.append(a_out)
        elif self.agg_height > 0:
            aligns_align = [0] * self.interconnect_input_ports

        if self.agg_height > 0:
            # Now send data to agg buffers
            data_agg_buff = []
            valid_agg_buff = []
            for i in range(self.interconnect_input_ports):
                # (dt, vt) = self.agg_buffs[i].insert(data_align[i], valids_align[i])
                (dt, vt) = self.agg_buffs[i].interact(data_align[i], valids_align[i], 0)
                data_agg_buff.append(dt)
                valid_agg_buff.append(vt)
                # data_agg_buff.append(self.agg_buffs[i].get_item())
                # valid_agg_buff.append(self.agg_buffs[i].get_valid_out())
        elif self.agg_height == 0:
            valid_agg_buff = wen
            data_agg_buff = data_in

        self.valid_agg_buff = valid_agg_buff

        # Now send agg_buff stuff to IAC
        (iac_valid, iac_data, iac_addrs, self.iac_port_out) = \
            self.iac.interact(self.valid_agg_buff, data_agg_buff, arb_wen_en)

        # Get prefetch step for OAC...
        pref_step = []
        arb_ack = 0
        for i in range(self.interconnect_output_ports):
            pref_step.append(self.prefetchers[i].get_step())

        # Rd sync gate for acks from rw arb
        rd_sync_gate = self.sync_groups.get_rd_sync()
        # Also get ren from oac
        oac_ren = self.oac.get_ren(pref_step)

        # OAC
        # First need to get rw_arb acks based on ren + sync group gate
        # Now we need to squash the acks
        ack_base = 0

        for i in range(self.banks):
            ren_local = []
            for j in range(self.interconnect_output_ports):
                ren_local.append(0)
            for j in range(self.interconnect_output_ports):
                ren_local[j] = ren_local[j] | (rd_sync_gate[j] & oac_ren[i][j])
            local_ack = self.rw_arbs[i].get_ack(iac_valid[i], ac_wen_en, ren_local, ac_ren_en)
            ack_base = (ack_base | local_ack)

        # Final interaction with OAC
        (oac_ren, oac_addrs) = self.oac.interact(valid_in=pref_step,
                                                 step_in=ack_base,
                                                 enable_chain_output=self.config[f"enable_chain_output"])

        # Get data from mem
        data_to_arb = []
        for i in range(self.banks):
            if self.read_delay == 1:
                data_to_arb.append(self.mems[i].get_rd_reg())
            else:
                data_to_arb.append(0)

        # RW arbs
        rw_out_dat = []
        rw_out_port = []
        rw_out_valid = []
        rw_cen_mem = []
        rw_wen_mem = []
        rw_data_to_mem = []
        rw_addr_to_mem = []
        rw_ack = []
        rw_mem_valid_data = []
        for i in range(self.banks):
            gated_ren = []
            for j in range(self.interconnect_output_ports):
                gated_ren.append(oac_ren[i][j] & rd_sync_gate[j])

            (od,
             op,
             ov,
             cm,
             wm,
             dm,
             am,
             ack,
             omvd) = self.rw_arbs[i].interact(iac_valid[i],
                                              ac_wen_en,
                                              iac_data[i],
                                              iac_addrs[i],
                                              data_to_arb[i],
                                              gated_ren,
                                              ac_ren_en,
                                              oac_addrs,
                                              self.mem_valid_data)

            rw_out_dat.append(od)
            rw_out_port.append(op)
            rw_out_valid.append(ov)
            rw_cen_mem.append(cm)
            rw_wen_mem.append(wm)
            rw_data_to_mem.append(dm)
            rw_addr_to_mem.append(am)
            rw_ack.append(ack)
            rw_mem_valid_data.append(omvd)

        self.mem_data_out = []
        self.mem_valid_data = []
        # SRAM Wrapper
        if self.read_delay == 1:
            for i in range(self.banks):
                sram_mdo, sram_mvd = self.mems[i].interact(rw_data_to_mem[i],
                                                           rw_addr_to_mem[i],
                                                           rw_cen_mem[i],
                                                           rw_wen_mem[i],
                                                           1,
                                                           1)
                self.mem_data_out.append(sram_mdo)
                self.mem_valid_data.append(sram_mvd)

        # Register File
        else:
            rw_out_dat = []
            for i in range(self.banks):
                local_rf_read = self.mems[i].interact(rw_wen_mem[i],
                                                      rw_addr_to_mem[i],
                                                      rw_addr_to_mem[i],
                                                      rw_data_to_mem[i])
                # Hack for one port
                rw_out_dat.append(local_rf_read[0])

        # Demux those reads
        (demux_dat, demux_valid, demux_mem_valid_data) = \
            self.demux_reads.interact(rw_out_dat,
                                      rw_out_valid,
                                      rw_out_port,
                                      rw_mem_valid_data)

        # Requires or'd version of ren
        ren_base = []
        for j in range(self.interconnect_output_ports):
            ren_base.append(0)
        for j in range(self.interconnect_output_ports):
            for i in range(self.banks):
                ren_base[j] = ren_base[j] | (oac_ren[i][j])

        # Sync groups now
        (sync_data, sync_valid, rd_sync_gate_x, sync_mem_valid_data) = \
            self.sync_groups.interact(ack_base,
                                      demux_dat,
                                      demux_valid,
                                      ren_base,
                                      demux_mem_valid_data)

        # Now get the tba rdy and interact with prefetcher
        tba_rdys = []
        for i in range(self.interconnect_output_ports):
            tba_rdys.append(self.tbas[i].get_ready())

        pref_data = []
        pref_valid = []
        for i in range(self.interconnect_output_ports):
            (pd, pv, psx, prefetcher_mem_valid_data) = \
                self.prefetchers[i].interact(sync_data[i],
                                             sync_valid[i],
                                             tba_rdys[i],
                                             sync_mem_valid_data[i])
            if type(pd) == list:
                pref_data.append(pd.copy())
            else:
                pref_data.append(pd)
            pref_valid.append(pv)

        # Now send this to the TBAs...
        data_out = []
        valid_out = []
        for i in range(self.interconnect_output_ports):
            (tb_d, tb_v) = self.tbas[i].tba_main(pref_data[i],
                                                 pref_valid[i],
                                                 pref_valid[i],
                                                 0,
                                                 ac_ren_out[i],
                                                 prefetcher_mem_valid_data[i])
            data_out.append(tb_d)
            valid_out.append(tb_v)

        self.tb_valid = valid_out

        # Chaining module is completely combinational,
        # just have it here as part of top

        curr_tile_data_out = data_out
        curr_tile_valid_out = valid_out

        chain_data_out_inter = []
        chain_valid_out_inter = []
        for i in range(self.interconnect_output_ports):
            if chain_valid_in[i] == 0:
                chain_data_out_inter.append(curr_tile_data_out[i])
                chain_valid_out_inter.append(curr_tile_valid_out[i])
            else:
                chain_data_out_inter.append(chain_data_in[i])
                chain_valid_out_inter.append(chain_valid_in[i])

        # all combinational outputs
        # set data_out_tile
        if self.enable_chain_output:
            data_out_tile = chain_data_out_inter
        else:
            data_out_tile = curr_tile_data_out

        # set valid_out_tile
        valid_out_tile = []
        if self.enable_chain_output:
            if not (self.chain_idx_output == 0):
                for i in range(self.interconnect_output_ports):
                    valid_out_tile.append(0)
            else:
                valid_out_tile = chain_valid_out_inter
        else:
            valid_out_tile = curr_tile_valid_out

        # set chain_data_out
        chain_data_out = chain_data_out_inter

        # set chain_valid_out
        chain_valid_out = []
        if (self.chain_idx_output == 0) or \
                (not self.enable_chain_output):
            for i in range(self.interconnect_output_ports):
                chain_valid_out.append(0)
        else:
            chain_valid_out = chain_valid_out_inter

        return (chain_data_out, chain_valid_out, data_out_tile, valid_out_tile)
