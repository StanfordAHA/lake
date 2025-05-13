from lake.spec.component import Component, lift_config_space
from lake.utils.spec_enum import *
import random as rand
from lake.spec.address_generator import AddressGenerator
from lake.spec.memory_port import MemoryPort
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator, ReadyValidScheduleGenerator
from lake.spec.storage import SingleBankStorage, Storage
from lake.modules.rv_comparison_network import RVComparisonNetwork
from lake.spec.reg_fifo import RegFIFO
from lake.utils.util import connect_memoryport_storage, add_counter, register, sticky_flag
import kratos as kts
from kratos import *


def print_class_hierarchy(obj):
    print("Class Hierarchy:")
    for cls in obj.__class__.__mro__:
        print(cls.__name__)


class Port(Component):

    def __init__(self, ext_data_width=16, int_data_width=16,
                 runtime=Runtime.STATIC, direction=Direction.IN,
                 vec_capacity=None, opt_rv=False,
                 opt_timing=True, filter=False,
                 dangling=False):
        super().__init__()
        self._mp_intf = {}
        self._ub_intf = {}
        self._ext_data_width = ext_data_width
        self._int_data_width = int_data_width
        self._runtime = runtime
        self._direction = direction
        self._fw = self._int_data_width // self._ext_data_width
        if self._fw != 1:
            assert vec_capacity is not None
        # Vec capacity is multiplied by the wider side
        self._vec_capacity = vec_capacity
        self.dimensionality = None
        self._internal_step = None
        self._rv_comp_nw = None
        self.opt_rv = opt_rv
        self.port_ag_width = None
        self.opt_timing = opt_timing
        #  We want to handle flush and clk_en ourselves
        self.sync_reset_no_touch = True
        self.clk_en_no_touch = True
        self.filter = filter

        if self._runtime == Runtime.STATIC:
            assert self.filter is False, "Filter not supported in static runtime"

        # Only allow dangling write ports for now - will prevent it from getting data at the input (so it won't need anything)
        if dangling:
            assert direction == Direction.IN
        self.dangling = dangling

    def __str__(self):
        type_str = "Write"
        if self._direction == Direction.OUT:
            type_str = "Read"
        return f"Port: {type_str}"

    def get_dangling(self):
        return self.dangling

    def set_port_ag_width(self, width):
        self.port_ag_width = width

    def set_dimensionality(self, dim):
        self.dimensionality = dim

    def get_internal_ag_intf(self):
        '''Only used in a vectorization context
        '''
        assert self._fw != 1
        return self._internal_ag_intf

    def get_fw(self):
        return self._fw

    def get_filter(self):
        return self.filter

    def gen_hardware(self, pos_reset=False, dimensionality=None, external_id=None):

        all_to_lift = []

        # self._clk = self.clock('clk')
        # self._rst_n = self.reset('rst_n')

        # Going to always have a ready/valid in

        if dimensionality is not None:
            self.set_dimensionality(dimensionality)

        if self._direction == Direction.IN:

            # This is the hardware for a single fetch width
            # in set
            # if self._runtime == Runtime.STATIC:
            #     data_from_ub = self.input(f"port_write_data_in", self._ext_data_width)
            #     data_to_memport = self.output(f"port_write_data_out", self._int_data_width)
            #     self._ub_intf['data'] = data_from_ub
            #     self._mp_intf['data'] = data_to_memport
            # else:
            data_from_ub = self.rvinput(name=f"port_write_data_in", width=self._ext_data_width, packed=True)
            data_to_memport = self.rvoutput(name=f"port_write_data_out", width=self._int_data_width)
            self._ub_intf['data'] = data_from_ub.get_port()
            self._ub_intf['valid'] = data_from_ub.get_valid()
            self._ub_intf['ready'] = data_from_ub.get_ready()
            self._mp_intf['data'] = data_to_memport.get_port()
            self._mp_intf['valid'] = data_to_memport.get_valid()
            self._mp_intf['ready'] = data_to_memport.get_ready()

            if self._fw == 1:
                # wire together
                if self._runtime == Runtime.STATIC:
                    # self.wire(data_from_ub, data_to_memport)
                    data_from_pintf = data_from_ub.get_port_interface()
                    data_to_pintf = data_to_memport.get_port_interface()
                    self.wire(data_from_pintf['data'], data_to_pintf['data'])
                    self.wire(data_from_pintf['valid'], data_to_pintf['valid'])
                    self.wire(data_from_pintf['ready'], data_to_pintf['ready'])
                else:

                    # Dynamic in...
                    data_from_pintf = data_from_ub.get_port_interface()
                    data_to_pintf = data_to_memport.get_port_interface()

                    # If we are filtering, need to add a schedule generator with count on the ready/valid transaction.
                    if self.filter:
                        assert self.dimensionality is not None
                        assert external_id is not None
                        external_id: IterationDomain
                        print("Filtering...")
                        self._filter_mux_sel = self.input("filter_mux_sel", max(kts.clog2(self.dimensionality), 1), packed=True)
                        self._filter_restart = self.input("filter_restart", 1, packed=True)
                        self._filter_finished = self.input("filter_finished", 1)
                        self._filter_iterators = self.input("filter_iterators", external_id.get_extent_width(),
                                                            size=self.dimensionality,
                                                            packed=True,
                                                            explicit_array=True)
                        filter_sg = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16, rv=False,
                                                      name="filter_sg", recurrence=True, external_count=True)

                        filter_sg.gen_hardware(id=external_id)

                        self.add_child(f"filter_sg_inst",
                                       filter_sg,
                                       clk=self._clk,
                                       rst_n=self._rst_n,
                                       clk_en=self._clk_en,
                                       flush=self._flush)

                        self.wire(self._filter_mux_sel, filter_sg.ports.mux_sel)
                        self.wire(self._filter_restart, filter_sg.ports.restart)
                        self.wire(self._filter_finished, filter_sg.ports.finished)
                        self.wire(self._filter_iterators, filter_sg.ports.iterators)
                        # assert False, "Dying on purpose..."

                        rv_transaction = self.var("rv_transaction_go", 1)
                        # self.wire(rv_transaction, data_from_pintf['valid'] & data_to_pintf['ready'])
                        # The rv transaction is fine to "proceed" (drop data) if the filter_sg.step is low, but needs the ready if it's high
                        self.wire(rv_transaction, data_from_pintf['valid'] & kts.ternary(filter_sg.ports.step, data_to_pintf['ready'], kts.const(1, 1)))
                        self.wire(rv_transaction, filter_sg.ports.external_count)

                        # The incoming valids will get eaten by the rv_transaction
                        # and so the SG's output step becomes the filtered valid for the downstream
                        self.wire(filter_sg.ports.step & data_from_pintf['valid'], data_to_pintf['valid'])

                        # The ready from the downstream is the one we pass up - since only the valid needs to be filtered out
                        # as it's a strict subset - this won't affect the ready from downstream nor does it need
                        # to be processed on the way to the upstream
                        # self.wire(data_from_pintf['ready'], data_to_pintf['ready'])
                        # self.wire(data_from_pintf['valid'], data_to_pintf['valid'])
                        self.filter_sg = filter_sg
                        # The ready out is the normal ready if the step is high otherwise it's 1 if it's low
                        self.wire(data_from_pintf['ready'], kts.ternary(filter_sg.ports.step, data_to_pintf['ready'], kts.const(1, 1)))

                    else:
                        self.wire(data_from_pintf['valid'], data_to_pintf['valid'])
                        self.wire(data_from_pintf['ready'], data_to_pintf['ready'])

                    self.wire(data_from_pintf['data'], data_to_pintf['data'])

            else:

                # Dynamic in (WIDE FETCH)
                if self._runtime == Runtime.DYNAMIC and self.opt_rv:

                    assert self.port_ag_width is not None

                    # Need the WCB itself
                    max_num_items_wcb = 2
                    total_values = max_num_items_wcb + 1
                    wcb_items_bits = kts.clog2(total_values)
                    self._ready_out_lcl = self.var("ready_out_lcl", 1)
                    ub_interface = data_from_ub.get_port_interface()
                    mp_interface = data_to_memport.get_port_interface()
                    # self.wire(self._ready_out_lcl, ub_interface['ready'])
                    # Need ID finished...

                    self._finished = self.input("finished", 1)

                    if self.opt_timing:

                        # Put a depth-2 FIFO at the input to break the comb loop
                        input_fifo = RegFIFO(data_width=ub_interface['data'].width, width_mult=1, depth=2)

                        self.add_child(f"input_fifo",
                                        input_fifo,
                                        clk=self._clk,
                                        rst_n=self._rst_n,
                                        clk_en=self._clk_en,
                                        flush=self._flush,
                                        push=ub_interface['valid'],
                                        # pop=self._pop_addr_q,
                                        data_in=ub_interface['data'])

                        self.wire(ub_interface['ready'], ~input_fifo.ports.full)

                        tmp_data_in = self.var("data_in_proxy", ub_interface['data'].width, packed=True)
                        tmp_valid_in = self.var("valid_in_proxy", 1)
                        tmp_ready_out = self.var("ready_out_proxy", 1)

                        self.wire(tmp_data_in, input_fifo.ports.data_out)
                        self.wire(input_fifo.ports.pop, tmp_ready_out)
                        self.wire(tmp_valid_in, ~input_fifo.ports.empty)

                        # Replace the interface so that the rest of the logic works the same...
                        ub_interface['data'] = tmp_data_in
                        ub_interface['valid'] = tmp_valid_in
                        ub_interface['ready'] = tmp_ready_out

                    # Filter the input
                    # If we are filtering, need to add a schedule generator with count on the ready/valid transaction.
                    if self.filter:
                        print("FILTERING!!!")
                        # assert False, "On purpose..."
                        assert self.dimensionality is not None
                        assert external_id is not None
                        external_id: IterationDomain
                        print("Filtering...")
                        self._filter_mux_sel = self.input("filter_mux_sel", max(kts.clog2(self.dimensionality), 1), packed=True)
                        self._filter_restart = self.input("filter_restart", 1, packed=True)
                        self._filter_finished = self.input("filter_finished", 1)
                        self._filter_iterators = self.input("filter_iterators", external_id.get_extent_width(),
                                                            size=self.dimensionality,
                                                            packed=True,
                                                            explicit_array=True)
                        filter_sg = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16, rv=False,
                                                      name="filter_sg", recurrence=True, external_count=True)

                        filter_sg.gen_hardware(id=external_id)

                        self.add_child(f"filter_sg_inst",
                                       filter_sg,
                                       clk=self._clk,
                                       rst_n=self._rst_n,
                                       clk_en=self._clk_en,
                                       flush=self._flush)

                        self.wire(self._filter_mux_sel, filter_sg.ports.mux_sel)
                        self.wire(self._filter_restart, filter_sg.ports.restart)
                        self.wire(self._filter_finished, filter_sg.ports.finished)
                        self.wire(self._filter_iterators, filter_sg.ports.iterators)
                        # assert False, "Dying on purpose..."

                        rv_transaction = self.var("rv_transaction_go", 1)
                        # Ub interface ready and valid anded...
                        # self.wire(rv_transaction, ub_interface['valid'] & ub_interface['ready'])
                        self.wire(rv_transaction, ub_interface['valid'] & kts.ternary(filter_sg.ports.step, ub_interface['ready'], kts.const(1, 1)))
                        self.wire(rv_transaction, filter_sg.ports.external_count)

                        # The incoming valids will get eaten by the rv_transaction
                        # and so the SG's output step becomes the filtered valid for the downstream

                        # Need to replace the valid...
                        ub_interface['valid'] = filter_sg.ports.step & ub_interface['valid']
                        # self.wire(filter_sg.ports.step, data_to_pintf['valid'])

                        # The ready from the downstream is the one we pass up - since only the valid needs to be filtered out
                        # as it's a strict subset - this won't affect the ready from downstream nor does it need
                        # to be processed on the way to the upstream

                        # assert False, "On purpose...MEK"
                        self.filter_sg = filter_sg
                        self.wire(ub_interface['ready'], kts.ternary(filter_sg.ports.step, self._ready_out_lcl & ~self._finished, kts.const(1, 1)))

                    else:
                        # Ready out needs to be the ready from the UB and the finished signal
                        self.wire(ub_interface['ready'], self._ready_out_lcl & ~self._finished)

                    # self.wire(self._data_out_lcl, ub_interface['data'])

                    self._full_addr_in = self.input("addr_in", width=self.port_ag_width)
                    # self._addr_out = self.output("addr_out", width=self.port_ag_width)
                    self._addr_out = self.output("addr_out", width=(self.port_ag_width - kts.clog2(self.get_fw())))
                    self._sg_step_in = self.input("sg_step_in", width=1)
                    # This will be used to send out to the AG/ID, etc.
                    # This is also telling us if we are making a memory read this cycle
                    self._sg_step_out = self.output("sg_step_out", width=1)
                    self._write_memory_out = self.output("write_memory_out", width=1)
                    self._write_memory_out_lcl = self.var("write_memory_out_lcl", width=1)
                    self._new_address = self.var("new_address", 1)
                    self.wire(self._write_memory_out, self._write_memory_out_lcl)
                    # self.wire(mp_interface['valid'], self._write_memory_out_lcl)
                    # Just always be requesting?
                    # self.wire(mp_interface['valid'], kts.const(1, 1))
                    # self.wire(self._write_memory_out_lcl, self._new_address)

                    # self._data_being_written = self.var("data_being_written", 1)

                    # Output address bits ragnge
                    addr_bits_range = [self._full_addr_in.width - 1, kts.clog2(self._fw)]

                    # Define all the relevant signals/vars
                    sub_addr_bits = kts.clog2(self._fw)
                    sram_addr_bits = self._full_addr_in.width - sub_addr_bits
                    sub_addr_range = [kts.clog2(self._fw) - 1, 0]
                    # tag_addr_range = [kts.clog2(self._fw) + tag_width - 1, kts.clog2(self._fw)]

                    # self.wire(self._addr_out, kts.concat(kts.const(0, sub_addr_bits), self._full_addr_in[self._full_addr_in.width - 1, self._full_addr_in.width - sram_addr_bits]))
                    # self.wire(self._addr_out, kts.concat(self._full_addr_in[self._full_addr_in.width - 1, self._full_addr_in.width - sub_addr_bits],
                    #                                      self._full_addr_in[self._full_addr_in.width - 1, self._full_addr_in.width - sram_addr_bits]))

                    # Need a signal to write to the wcb
                    self._write_wcb = self.var("write_wcb", 1)
                    self._already_written = sticky_flag(self, self._write_wcb, name="already_written", seq_only=True, clear=self._flush, clk_en=self._clk_en)
                    # The ready out is if we are writing the wcb which means we are accepting the current data
                    self.wire(self._ready_out_lcl, self._write_wcb)
                    self.wire(self._sg_step_out, self._write_wcb)

                    # Indicates if the current address is a new address or not - either if "new_address" or hasn't been written yet and write_wcb is high
                    # self._last_write_addr = register(self, self._full_addr_in, enable=self._new_address, name="last_write_addr", clear=self._flush, clk_en=self._clk_en)
                    self._last_write_addr = register(self, self._full_addr_in, enable=kts.ternary(self._already_written, self._new_address, self._write_wcb), name="last_write_addr", clear=self._flush, clk_en=self._clk_en)

                    # Linear write address to the WCB
                    # Need push and pop addresses to manage the WCB
                    self._linear_wcb_write = add_counter(self, "linear_wcb_write", bitwidth=kts.clog2(self._vec_capacity), increment=self._new_address, clear=self._flush, clk_en=self._clk_en)
                    self._linear_wcb_write_p1 = self.var("linear_wcb_write_p1", self._linear_wcb_write.width)
                    self.wire(self._linear_wcb_write_p1, self._linear_wcb_write + 1)
                    self._linear_wcb_read = add_counter(self, "linear_wcb_read", bitwidth=kts.clog2(self._vec_capacity), increment=self._write_memory_out_lcl, clear=self._flush, clk_en=self._clk_en)
                    # Need an address into the wcb (should be the linear_wcb_write concatenated with the sub_addr)
                    self._addr_into_wcb = self.var("addr_into_wcb", self._linear_wcb_write.width + sub_addr_bits)
                    # self.wire(self._addr_into_wcb, kts.concat(kts.ternary(self._new_address,
                    #                                                       self._linear_wcb_write_p1,
                    #                                                       self._linear_wcb_write),
                    #                                           self._full_addr_in[sub_addr_range[0], sub_addr_range[1]]))

                    wcb_write_addr_bits = kts.clog2(max_num_items_wcb)
                    wcb_write_addr_range = [sub_addr_bits + wcb_write_addr_bits - 1, sub_addr_bits]

                    # Combinational logic to determine the address into the wcb...
                    @always_comb
                    def addr_into_wcb_comb():
                        self._addr_into_wcb = 0
                        self._addr_into_wcb[sub_addr_range[0], sub_addr_range[1]] = self._full_addr_in[sub_addr_range[0], sub_addr_range[1]]
                        # If we've never written, use the current wcb write address, if it has been written, we should use new_address to determine
                        if ~self._already_written:
                            self._addr_into_wcb[wcb_write_addr_range[0], wcb_write_addr_range[1]] = self._linear_wcb_write
                        elif self._new_address:
                            self._addr_into_wcb[wcb_write_addr_range[0], wcb_write_addr_range[1]] = self._linear_wcb_write_p1
                        else:
                            self._addr_into_wcb[wcb_write_addr_range[0], wcb_write_addr_range[1]] = self._linear_wcb_write
                    self.add_code(addr_into_wcb_comb)

                    ###############################################################
                    ###############################################################
                    ###############################################################
                    # This section creates the actual hardware for the SIPO storage
                    ###############################################################
                    if self._ext_data_width > self._int_data_width:
                        storage_vec_cap = self._ext_data_width * self._vec_capacity // 8
                    else:
                        storage_vec_cap = self._int_data_width * self._vec_capacity // 8

                    # Create PISO storage and then tie it to the relevant memoryports
                    self._sipo_strg = SingleBankStorage(capacity=storage_vec_cap)
                    self._sipo_strg_mp_in = MemoryPort(data_width=self._ext_data_width, mptype=MemoryPortType.W)
                    self._sipo_strg_mp_out = MemoryPort(data_width=self._int_data_width, mptype=MemoryPortType.R, delay=0, active_read=False)

                    memports = [self._sipo_strg_mp_in, self._sipo_strg_mp_out]

                    self._sipo_strg.gen_hardware(memory_ports=memports)
                    self._sipo_strg_mp_in.gen_hardware(storage_node=self._sipo_strg)
                    self._sipo_strg_mp_out.gen_hardware(storage_node=self._sipo_strg)

                    strg_intfs = self._sipo_strg.get_memport_intfs()

                    self.add_child('vec_storage', self._sipo_strg, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                    self.add_child('vec_storage_mp_in', self._sipo_strg_mp_in, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                    self.add_child('vec_storage_mp_out', self._sipo_strg_mp_out, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)

                    connect_memoryport_storage(self, mptype=self._sipo_strg_mp_in.get_type(),
                                               memport_intf=self._sipo_strg_mp_in.get_storage_intf(),
                                               strg_intf=strg_intfs[0])
                    connect_memoryport_storage(self, mptype=self._sipo_strg_mp_out.get_type(),
                                               memport_intf=self._sipo_strg_mp_out.get_storage_intf(),
                                               strg_intf=strg_intfs[1])

                    # We also need a set of regs that are going to hold the address and valid bit (sticky) meaning
                    # we can send the write to the SRAM
                    addresses_to_write = self.var("addresses_to_write", sram_addr_bits,
                                               size=max_num_items_wcb, packed=True, explicit_array=True)
                    next_addresses_to_write = self.var("next_addresses_to_write", sram_addr_bits,
                                               size=max_num_items_wcb, packed=True, explicit_array=True)

                    self._write_can_commit_clr = self.var("write_can_commit_clr", max_num_items_wcb)
                    self._write_can_commit_set = self.var("write_can_commit_set", max_num_items_wcb)

                    write_can_commit_sticky_pre = [sticky_flag(self, self._write_can_commit_set[i], name=f"write_can_commit_sticky_{i}", seq_only=True,
                                                           clear=self._write_can_commit_clr[i] | self._flush, clk_en=self._clk_en) for i in range(max_num_items_wcb)]

                    self._write_can_commit_sticky = self.var("write_can_commit_sticky", max_num_items_wcb)
                    [self.wire(self._write_can_commit_sticky[i], write_can_commit_sticky_pre[i]) for i in range(max_num_items_wcb)]

                    # self.add_code(tag_valid_cam_ff)
                    # self.add_code(next_tag_valid_cam_comb)

                    @always_comb
                    def next_addresses_to_write_comb():
                        for i in range(max_num_items_wcb):
                            next_addresses_to_write[i] = addresses_to_write[i]
                            if self._write_can_commit_set[i]:
                                next_addresses_to_write[i] = self._last_write_addr[addr_bits_range[0], addr_bits_range[1]]

                    self.add_code(next_addresses_to_write_comb)

                    @always_ff((posedge, "clk"), (negedge, "rst_n"))
                    def addresses_to_write_ff():
                        if ~self._rst_n:
                            for i in range(max_num_items_wcb):
                                addresses_to_write[i] = 0
                        elif self._flush:
                            for i in range(max_num_items_wcb):
                                addresses_to_write[i] = 0
                        elif self._clk_en:
                            for i in range(max_num_items_wcb):
                                if self._write_can_commit_set[i]:
                                    addresses_to_write[i] = next_addresses_to_write[i]

                    self.add_code(addresses_to_write_ff)

                    # Address out is whichever address is being pointed to by the linear read address
                    # self.wire(self._addr_out, kts.concat(self._full_addr_in[self._full_addr_in.width - 1, self._full_addr_in.width - sub_addr_bits],
                    #  self._full_addr_in[self._full_addr_in.width - 1, self._full_addr_in.width - sram_addr_bits]))
                    # self.wire(self._addr_out, kts.concat(self._last_write_addr[self._full_addr_in.width - 1, self._full_addr_in.width - sub_addr_bits],
                    #                                      addresses_to_write[self._linear_wcb_read]))
                    self.wire(self._addr_out, addresses_to_write[self._linear_wcb_read])

                    # write_memory_out is just the mp_interface's ready and the sticky bit
                    self.wire(self._write_memory_out_lcl, self._mp_intf['ready'] & self._write_can_commit_sticky[self._linear_wcb_read])

                    wp_intf = self._sipo_strg_mp_in.get_port_intf()
                    rp_intf = self._sipo_strg_mp_out.get_port_intf()

                    # Wire in the inputs and outputs...
                    self.wire(ub_interface['data'], wp_intf['write_data'])
                    self.wire(self._write_wcb, wp_intf['write_en'])
                    self.wire(self._addr_into_wcb, wp_intf['addr'])

                    self.wire(rp_intf['read_data'], mp_interface['data'])
                    self.wire(self._write_memory_out_lcl, rp_intf['read_en'])
                    # Also handle address
                    self.wire(self._linear_wcb_read, rp_intf['addr'])

                    # self.wire(mp_interface['valid'], kts.const(1, 1))
                    self.wire(mp_interface['valid'], self._write_can_commit_sticky[self._linear_wcb_read])
                    ###############################################################
                    ###############################################################
                    ###############################################################

                    @always_comb
                    def write_port_comb_logic():
                        # Need to set the writing wcb
                        self._write_wcb = 0
                        # We should write it as long as there is no valid bit or the valid bit is high but the entry is being written (and the input data is valid)
                        # to the SRAM
                        if self._sg_step_in & ub_interface['valid'] & (~self._write_can_commit_sticky[self._linear_wcb_write] | ~self._already_written | (self._write_can_commit_sticky[self._linear_wcb_write] & self._write_memory_out & (self._linear_wcb_read == self._linear_wcb_write))):
                            self._write_wcb = ~self._finished

                    self.add_code(write_port_comb_logic)

                    @always_comb
                    def sticky_set_comb():
                        # The set can only happen to the current write address if we see a new address and have valid data and the step in
                        for i in range(max_num_items_wcb):
                            self._write_can_commit_set[i] = 0
                            # if (self._linear_wcb_write == kts.const(i, self._linear_wcb_write.width)) & self._new_address & ub_interface['valid'] & self._sg_step_in:
                            # Once the stream is finished, we no longer need a new address w/ incoming valid/step
                            if (self._linear_wcb_write == kts.const(i, self._linear_wcb_write.width)) & ((self._new_address & ub_interface['valid'] & self._sg_step_in) | self._finished):
                                self._write_can_commit_set[i] = 1
                    self.add_code(sticky_set_comb)

                    @always_comb
                    def sticky_clr_comb():
                        # Can only clear the sticky bit if we are writing the data to sram
                        for i in range(max_num_items_wcb):
                            self._write_can_commit_clr[i] = 0
                            if (self._linear_wcb_read == kts.const(i, self._linear_wcb_read.width)) & mp_interface['ready'] & self._write_memory_out:
                                self._write_can_commit_clr[i] = 1
                    self.add_code(sticky_clr_comb)

                    @always_comb
                    def new_address_comb():
                        self._new_address = 0
                        # self._new_address = (self._full_addr_in[addr_bits_range[0], addr_bits_range[1]] != self._last_write_addr[addr_bits_range[0], addr_bits_range[1]]) & self._sg_step_in & ub_interface['valid']
                        # Only a new address if we have already written - this accounts for write addresses that don't start at 0
                        self._new_address = (self._full_addr_in[addr_bits_range[0], addr_bits_range[1]] != self._last_write_addr[addr_bits_range[0], addr_bits_range[1]]) & self._sg_step_in & ub_interface['valid'] & self._already_written

                    self.add_code(new_address_comb)

                    # Now lift everything's config space up
                    self.config_space_fixed = True
                    self._assemble_cfg_memory_input()

                    return

                # Need to create a n rv network here...
                if self._runtime == Runtime.DYNAMIC:
                    self._rv_comp_nw = RVComparisonNetwork(name=f"rv_comp_nw_{self.name}")

                # Cannot build the hardware without this information
                assert self.dimensionality is not None
                assert external_id is not None
                # For the write port with a wide fetch, we need
                # storage and memory ports for SIPO, ID, AG, SG (original size + 1) on the front
                # Capacity of the storage will be w.r.t. wider width

                print("vec cap")
                print(self._vec_capacity)
                # Storage takyes bytes, widths are in bits
                if self._ext_data_width > self._int_data_width:
                    storage_vec_cap = self._ext_data_width * self._vec_capacity // 8
                else:
                    storage_vec_cap = self._int_data_width * self._vec_capacity // 8
                self._sipo_strg = SingleBankStorage(capacity=storage_vec_cap)
                self._sipo_strg_mp_in = MemoryPort(data_width=self._ext_data_width, mptype=MemoryPortType.W)
                self._sipo_strg_mp_out = MemoryPort(data_width=self._int_data_width, mptype=MemoryPortType.R, delay=0, active_read=False)

                memports = [self._sipo_strg_mp_in, self._sipo_strg_mp_out]

                self._sipo_strg.gen_hardware(memory_ports=memports)
                self._sipo_strg_mp_in.gen_hardware(storage_node=self._sipo_strg)
                self._sipo_strg_mp_out.gen_hardware(storage_node=self._sipo_strg)

                strg_intfs = self._sipo_strg.get_memport_intfs()

                self.add_child('vec_storage', self._sipo_strg, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                self.add_child('vec_storage_mp_in', self._sipo_strg_mp_in, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                self.add_child('vec_storage_mp_out', self._sipo_strg_mp_out, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)

                connect_memoryport_storage(self, mptype=self._sipo_strg_mp_in.get_type(),
                                           memport_intf=self._sipo_strg_mp_in.get_storage_intf(),
                                           strg_intf=strg_intfs[0])
                connect_memoryport_storage(self, mptype=self._sipo_strg_mp_out.get_type(),
                                           memport_intf=self._sipo_strg_mp_out.get_storage_intf(),
                                           strg_intf=strg_intfs[1])
                # Generate the storage and connect the memory ports

                # Now that we have built the storage and MemoryPort interfaces, we can build the extra controllers and wire them
                # to the memory ports
                # Input side stuff
                self._id_sipo_in = IterationDomain(dimensionality=self.dimensionality)
                if self._runtime == Runtime.DYNAMIC:
                    self._sg_sipo_in = ReadyValidScheduleGenerator(dimensionality=self.dimensionality)
                    self._sg_sipo_out = ReadyValidScheduleGenerator(dimensionality=self.dimensionality)
                else:
                    self._sg_sipo_in = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16)
                self._ag_sipo_in = AddressGenerator(dimensionality=self.dimensionality)
                # Output side stuff - don't actually need the ID, SG, can just use the ID, SG from the original spec which will be built to
                # drive the Storage element anyway. Just need to accept a step/enable signal in from the outside anyway
                # self._id_int = IterationDomain(dimensionality=self.dimensionality)
                # self._sg_int = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16)

                # id_dims = port_id.get_dimensionality()
                # memports_ = self.get_memory_ports(port=port)
                # Port needs to know about the dimensionality in case of a vectorized port to
                # build the proper hardware within the port
                # port.gen_hardware(dimensionality=id_dims)
                self._id_sipo_in.gen_hardware()
                self._sg_sipo_in.gen_hardware(self._id_sipo_in)

                # Register all sgs with the rv comp network
                if self._runtime == Runtime.DYNAMIC:
                    self._sg_sipo_out.gen_hardware(external_id, num_comparisons=1)
                    self.add_child('port_sg_sipo_out', self._sg_sipo_out,
                                   clk=self._clk,
                                   rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                    self._rv_comp_nw.add_reader_writer(direction=Direction.IN, sg=self._sg_sipo_in)
                    self._rv_comp_nw.add_reader_writer(direction=Direction.OUT, sg=self._sg_sipo_out)

                self._ag_sipo_in.gen_hardware(memports=[self._sipo_strg_mp_in], id=self._id_sipo_in)

                # Connect the ag/sg/id together
                self.add_child(f"port_id_sipo_in", self._id_sipo_in,
                               clk=self._clk,
                               rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                self.add_child(f"port_ag_sipo_in", self._ag_sipo_in,
                               clk=self._clk,
                               rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                self.add_child(f"port_sg_sipo_in", self._sg_sipo_in,
                               clk=self._clk,
                               rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)

                self.wire(self._id_sipo_in.ports.mux_sel, self._ag_sipo_in.ports.mux_sel)
                self.wire(self._id_sipo_in.ports.iterators, self._ag_sipo_in.ports.iterators)
                self.wire(self._id_sipo_in.ports.restart, self._ag_sipo_in.ports.restart)
                self.wire(self._id_sipo_in.ports.finished, self._ag_sipo_in.ports.finished)

                self.wire(self._id_sipo_in.ports.mux_sel, self._sg_sipo_in.ports.mux_sel)
                self.wire(self._id_sipo_in.ports.iterators, self._sg_sipo_in.ports.iterators)
                self.wire(self._id_sipo_in.ports.restart, self._sg_sipo_in.ports.restart)
                self.wire(self._id_sipo_in.ports.finished, self._sg_sipo_in.ports.finished)

                if self._runtime == Runtime.DYNAMIC:
                    # The sipo in should only get stepped if the local step is high
                    # and the input data is valid
                    self.wire(self._sg_sipo_in.ports.step & self._ub_intf['valid'], self._ag_sipo_in.ports.step)
                    self.wire(self._sg_sipo_in.ports.step & self._ub_intf['valid'], self._id_sipo_in.ports.step)
                else:
                    self.wire(self._sg_sipo_in.ports.step, self._ag_sipo_in.ports.step)
                    self.wire(self._sg_sipo_in.ports.step, self._id_sipo_in.ports.step)

                # Send through extents if RV
                if self._runtime == Runtime.DYNAMIC:
                    self.wire(self._id_sipo_in.ports.extents_out, self._sg_sipo_in.ports.extents)

                assembled_port = {}
                assembled_port['data'] = self._ub_intf['data']
                assembled_port['addr'] = self._ag_sipo_in.get_address()
                # The data and addr to the SIPO can just be directly wired, but we need to qualify the write enable
                # with the SG's step and input valid
                if self._runtime == Runtime.DYNAMIC:
                    # Can also hook up the ub ready now - it's just the step since there's no arbitration on the MP
                    assembled_port['en'] = self._sg_sipo_in.get_step() & self._ub_intf['valid']
                    self.wire(self._ub_intf['ready'], self._sg_sipo_in.get_step())
                else:
                    assembled_port['en'] = self._sg_sipo_in.get_step()
                    self.wire(self._ub_intf['ready'], self._sg_sipo_in.get_step())
                    # self.wire(self._ub_intf['ready'], kts.const(1, 1))

                # Now hook up the input and output ports
                in_intf = self._sipo_strg_mp_in.get_port_intf()
                print(in_intf)
                self.wire(assembled_port['data'], in_intf['write_data'])
                self.wire(assembled_port['addr'], in_intf['addr'])
                self.wire(assembled_port['en'], in_intf['write_en'])

                # Need to add the AG ports to the port interface to get connected later
                self._ag_sipo_out = AddressGenerator(dimensionality=self.dimensionality)
                self._ag_sipo_out.gen_hardware(memports=[self._sipo_strg_mp_out], id=external_id)
                self._internal_ag_intf = {}

                self.add_child('internal_ag_with_ext_intf', self._ag_sipo_out, clk=self._clk,
                               rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                # step_tmp = self.input(f"port_vec_internal_step", 1)
                # self._internal_step['mux_sel'] = self.input(f"port_vec_internal_mux_sel", 1)
                # self._internal_step['iterators'] = self.input(f"port_vec_internal_dim_ctrs", 1)

                self._internal_ag_intf['step'] = self.input(f"port_sipo_out_step", 1)
                self._internal_ag_intf['restart'] = self.input(f"port_sipo_out_restart", 1)
                self._internal_ag_intf['finished'] = self.input(f"port_sipo_out_finished", 1)
                self._internal_ag_intf['mux_sel'] = self.input(f"port_sipo_out_mux_sel", self._ag_sipo_out.ports.mux_sel.width)
                self._internal_ag_intf['iterators'] = self.input(f"port_sipo_out_dim_ctrs", self._ag_sipo_out.ports.iterators.width,
                                                                 size=self.dimensionality, packed=True, explicit_array=True)
                # self.wire(self._internal_ag_intf['step'], self._ag_sipo_out.ports.step)
                self.wire(self._internal_ag_intf['mux_sel'], self._ag_sipo_out.ports.mux_sel)
                self.wire(self._internal_ag_intf['iterators'], self._ag_sipo_out.ports.iterators)
                self.wire(self._internal_ag_intf['restart'], self._ag_sipo_out.ports.restart)
                self.wire(self._internal_ag_intf['finished'], self._ag_sipo_out.ports.finished)
                if self._runtime == Runtime.DYNAMIC:
                    self._internal_ag_intf['extents'] = self.input("port_sipo_out_extents", external_id.get_extent_width(),
                                                                   size=external_id.get_dimensionality(), packed=True, explicit_array=True)
                    self.wire(self._internal_ag_intf['extents'], self._sg_sipo_out.ports.extents)
                    self.wire(self._internal_ag_intf['iterators'], self._sg_sipo_out.ports.iterators)
                    self.wire(self._internal_ag_intf['mux_sel'], self._sg_sipo_out.ports.mux_sel)
                    self.wire(self._internal_ag_intf['restart'], self._sg_sipo_out.ports.restart)
                    self.wire(self._internal_ag_intf['finished'], self._sg_sipo_out.ports.finished)
                    # Only step the address generator if the output is valid and
                    self.wire(self._internal_ag_intf['step'] & self._mp_intf['valid'] & self._mp_intf['ready'], self._ag_sipo_out.ports.step)
                else:
                    self.wire(self._internal_ag_intf['step'], self._ag_sipo_out.ports.step)

                assembled_port = {}
                assembled_port['data'] = self._mp_intf['data']
                assembled_port['addr'] = self._ag_sipo_out.get_address()

                # The data and addr out of the SIPO can just be directly wired, but we need to qualify the read enable
                # with the SG's step and output ready
                if self._runtime == Runtime.DYNAMIC:
                    # Valid is the same as en
                    assembled_port['en'] = self._sg_sipo_out.get_step()
                    self.wire(self._mp_intf['valid'], self._sg_sipo_out.get_step())
                    # assembled_port['en'] = self._internal_ag_intf['step'] & self._mp_intf['ready'] & self._sg_sipo_out.get_step()
                    # self.wire(self._mp_intf['valid'], self._internal_ag_intf['step'] & self._mp_intf['ready'] & self._sg_sipo_out.get_step())
                else:
                    # assembled_port['en'] = self._sg_sipo_in.get_step()
                    assembled_port['en'] = self._internal_ag_intf['step']
                    self.wire(self._mp_intf['valid'], self._internal_ag_intf['step'])

                out_intf = self._sipo_strg_mp_out.get_port_intf()
                print(out_intf)
                self.wire(assembled_port['data'], out_intf['read_data'])
                self.wire(assembled_port['addr'], out_intf['addr'])
                self.wire(assembled_port['en'], out_intf['read_en'])
                # self._internal_step['mux_sel'] = self.input(f"port_vec_internal_mux_sel", self._ag_int.ports.mux_sel.width)
                # self._internal_step['iterators'] = self.input(f"port_vec_internal_dim_ctrs", self._ag_int.ports.iterators.width)
                # 2xAG between for read of SIPO and Write of SRAM, ID, SG of original size

                # Now lift the config spaces
                all_to_lift = [self._sipo_strg, self._sipo_strg_mp_in, self._sipo_strg_mp_out,
                               self._id_sipo_in, self._sg_sipo_in, self._ag_sipo_in, self._ag_sipo_out]

                # Instantiate and hook up local rv comp network
                if self._runtime == Runtime.DYNAMIC:
                    self._rv_comp_nw.gen_hardware()
                    self.add_child('rv_comp_network_sipo', self._rv_comp_nw,
                                   clk=self._clk,
                                   rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                    rv_comp_conns = self._rv_comp_nw.get_connections()
                    for conn_tuple in rv_comp_conns:
                        p1, p2 = conn_tuple
                        self.wire(p1, p2)

        elif self._direction == Direction.OUT:

            # if self._runtime == Runtime.STATIC:
            #     data_from_memport = self.input(f"port_read_data_in", self._int_data_width)
            #     data_to_ub = self.output(f"port_read_data_out", self._ext_data_width)
            #     self._ub_intf['data'] = data_to_ub
            #     self._mp_intf['data'] = data_from_memport
            # else:
            data_from_memport = self.rvinput(name=f"port_read_data_in", width=self._int_data_width)
            data_to_ub = self.rvoutput(name=f"port_read_data_out", width=self._ext_data_width)
            self._ub_intf['data'] = data_to_ub.get_port()
            self._ub_intf['valid'] = data_to_ub.get_valid()
            self._ub_intf['ready'] = data_to_ub.get_ready()
            self._mp_intf['data'] = data_from_memport.get_port()
            self._mp_intf['valid'] = data_from_memport.get_valid()
            self._mp_intf['ready'] = data_from_memport.get_ready()

            if self._fw == 1:
                # wire together
                if self._runtime == Runtime.STATIC:
                    # self.wire(data_from_memport, data_to_ub)
                    data_from_pintf = data_to_ub.get_port_interface()
                    data_to_pintf = data_from_memport.get_port_interface()
                    self.wire(data_from_pintf['data'], data_to_pintf['data'])
                    self.wire(data_from_pintf['valid'], data_to_pintf['valid'])
                    self.wire(data_from_pintf['ready'], data_to_pintf['ready'])
                else:
                    data_from_pintf = data_to_ub.get_port_interface()
                    data_to_pintf = data_from_memport.get_port_interface()
                    self.wire(data_from_pintf['data'], data_to_pintf['data'])
                    self.wire(data_from_pintf['valid'], data_to_pintf['valid'])
                    self.wire(data_from_pintf['ready'], data_to_pintf['ready'])
            else:

                # Here we are going to implement the monolithic output path
                if self._runtime == Runtime.DYNAMIC and self.opt_rv:

                    # Will need the address and enable/step from the external AG/SG/ID
                    assert self.port_ag_width is not None
                    self._full_addr_in = self.input("addr_in", width=self.port_ag_width)
                    self._addr_out = self.output("addr_out", width=(self.port_ag_width - kts.clog2(self.get_fw())))
                    self._sg_step_in = self.input("sg_step_in", width=1)
                    self._grant = self.input("grant", width=1)
                    # This will be used to send out to the AG/ID, etc.
                    # This is also telling us if we are making a memory read this cycle
                    self._sg_step_out = self.output("sg_step_out", width=1)
                    self._read_memory_out = self.output("read_memory_out", width=1)
                    self._data_being_written = self.var("data_being_written", 1)

                    # This means there is no more data to be written to the SRAM
                    self._finished = self.input("finished", 1)

                    # self._inc_linear_wcb_write = self.var("inc_linear_wcb_write", 1)

                    # self._last_read_addr = self.var("last_read_addr", 1)
                    # self._read_last_cycle = self.var("read_last_cycle", 1)
                    # self._already_read = self.var("already_read", 1)
                    addr_bits_range = [self._full_addr_in.width - 1, kts.clog2(self._fw)]

                    self._already_read = sticky_flag(self, self._read_memory_out & self._grant, name="already_read", seq_only=True, clear=self._flush, clk_en=self._clk_en)
                    self._read_last_cycle = register(self, self._read_memory_out & self._grant, enable=kts.const(1, 1), name="read_last_cycle", clear=self._flush, clk_en=self._clk_en)
                    self._last_read_addr = register(self, self._full_addr_in, enable=self._read_memory_out & self._grant, name="last_read_addr", clear=self._flush, clk_en=self._clk_en)

                    # Define all the relevant signals/vars
                    addr_q_width = kts.clog2(self.get_fw())
                    pop_q_width = 1
                    # tag_width = kts.clog2(self._vec_capacity // 8)
                    tag_width = 1
                    sub_addr_range = [kts.clog2(self._fw) - 1, 0]
                    tag_addr_range = [kts.clog2(self._fw) + tag_width - 1, kts.clog2(self._fw)]

                    print(f"Tag width: {tag_width}")
                    # exit()

                    # self.wire(self._addr_out, kts.concat(self._full_addr_in >> addr_q_width))
                    self.wire(self._addr_out, self._full_addr_in[self._full_addr_in.width - 1, addr_q_width])

                    # In and out of addr queue
                    self._addr_q_sub_addr_in = self.var("addr_q_sub_addr_in", addr_q_width)
                    self._addr_q_sub_addr_out = self.var("addr_q_sub_addr_out", addr_q_width)
                    self._addr_q_pop_wcb_in = self.var("addr_q_pop_wcb_in", pop_q_width)
                    self._addr_q_pop_wcb_out = self.var("addr_q_pop_wcb_out", pop_q_width)
                    self._addr_q_tag_in = self.var("addr_q_tag_in", tag_width)
                    self._addr_q_tag_out = self.var("addr_q_tag_out", tag_width)

                    # Can pop the address queue and wcb separately - will only push a 1 once there are two items in the queue
                    self._pop_addr_q = self.var("pop_addr_q", 1)
                    self._push_addr_q = self.var("push_addr_q", 1)
                    self._pop_wcb = self.var("pop_wcb", 1)

                    # This tells us if there is still valid data in the skid buffer waiting to be written into the PISO
                    self._data_on_bus = self.var("data_on_bus", 1)
                    self._next_data_on_bus = self.var("next_data_on_bus", 1)

                    @always_ff((posedge, "clk"), (negedge, "rst_n"))
                    def data_on_bus_ff():
                        if ~self._rst_n:
                            self._data_on_bus = 0
                        elif self._flush:
                            self._data_on_bus = 0
                        elif self._clk_en:
                            self._data_on_bus = self._next_data_on_bus
                    self.add_code(data_on_bus_ff)

                    max_num_items_wcb = 2

                    total_values = max_num_items_wcb + 1
                    wcb_items_bits = kts.clog2(total_values)

                    # Calculate number of items in the wcb
                    self._num_items_wcb = self.var("num_items_wcb", wcb_items_bits)
                    self._next_num_items_wcb = self.var("next_num_items_wcb", wcb_items_bits)

                    @always_ff((posedge, "clk"), (negedge, "rst_n"))
                    def num_items_wcb_ff():
                        if ~self._rst_n:
                            self._num_items_wcb = 0
                        elif self._flush:
                            self._num_items_wcb = 0
                        elif self._clk_en:
                            self._num_items_wcb = self._next_num_items_wcb
                    self.add_code(num_items_wcb_ff)

                    self._valid_out_lcl = self.var("valid_out_lcl", 1)
                    self._data_out_lcl = self.var("data_out_lcl", self._ext_data_width)

                    # We will need an extra register to hold the data from the SRAM in the case
                    # that there is another Port attached to the same memory port - we can read from it
                    # directly if we made a read the previous cycle, but we will still register it for
                    # future cycles

                    # Signal to hold the data from the SRAM
                    self._register_data_from_mp = self.var("register_data_from_mp", 1)
                    # Get interface to MemoryPort
                    mp_intf = data_from_memport.get_port_interface()

                    # We can set the ready on the mp_intf to 1 since we can always accept data in the monolithic case
                    self.wire(mp_intf['ready'], kts.const(1, 1))

                    # Register the data from the MP
                    self._data_from_mp_reg = register(self, mp_intf['data'], enable=self._register_data_from_mp, name="data_from_mp_reg", clear=self._flush, clk_en=self._clk_en)
                    # If we read from SRAM last cycle, can use the data from the SRAM, otherwise we need to grab it from the local register
                    self._pick_input_data = kts.ternary(self._read_last_cycle, mp_intf['data'], self._data_from_mp_reg)

                    # We should register the data if we have incoming valid data but not writing it to the WCB
                    self.wire(self._register_data_from_mp, mp_intf['valid'] & ~self._data_being_written)

                    # Port interface's valid is the local valid
                    # The data is from the WCB
                    ub_interface = data_to_ub.get_port_interface()
                    self.wire(self._valid_out_lcl, ub_interface['valid'])
                    self.wire(self._data_out_lcl, ub_interface['data'])

                    # Linear write address to the WCB
                    # self._linear_wcb_write = self.var("linear_wcb_write", kts.clog2(self._vec_capacity))
                    self._linear_wcb_write = add_counter(self, "linear_wcb_write", bitwidth=kts.clog2(self._vec_capacity), increment=self._data_being_written, clear=self._flush, clk_en=self._clk_en)
                    self._linear_wcb_read = add_counter(self, "linear_wcb_read", bitwidth=kts.clog2(self._vec_capacity), increment=self._pop_wcb, clear=self._flush, clk_en=self._clk_en)

                    ###############################################################
                    ###############################################################
                    ###############################################################
                    # This section creates the actual hardware for the PISO storage
                    ###############################################################
                    if self._ext_data_width > self._int_data_width:
                        storage_vec_cap = self._ext_data_width * self._vec_capacity // 8
                    else:
                        storage_vec_cap = self._int_data_width * self._vec_capacity // 8

                    # Create PISO storage and then tie it to the relevant memoryports
                    self._piso_strg = SingleBankStorage(capacity=storage_vec_cap)
                    self._piso_strg_mp_in = MemoryPort(data_width=self._int_data_width, mptype=MemoryPortType.W)
                    self._piso_strg_mp_out = MemoryPort(data_width=self._ext_data_width, mptype=MemoryPortType.R, delay=0, active_read=False)

                    memports = [self._piso_strg_mp_in, self._piso_strg_mp_out]

                    self._piso_strg.gen_hardware(memory_ports=memports)
                    self._piso_strg_mp_in.gen_hardware(storage_node=self._piso_strg)
                    self._piso_strg_mp_out.gen_hardware(storage_node=self._piso_strg)

                    strg_intfs = self._piso_strg.get_memport_intfs()

                    self.add_child('vec_storage', self._piso_strg, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                    self.add_child('vec_storage_mp_in', self._piso_strg_mp_in, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                    self.add_child('vec_storage_mp_out', self._piso_strg_mp_out, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)

                    connect_memoryport_storage(self, mptype=self._piso_strg_mp_in.get_type(),
                                               memport_intf=self._piso_strg_mp_in.get_storage_intf(),
                                               strg_intf=strg_intfs[0])
                    connect_memoryport_storage(self, mptype=self._piso_strg_mp_out.get_type(),
                                               memport_intf=self._piso_strg_mp_out.get_storage_intf(),
                                               strg_intf=strg_intfs[1])

                    # We also need a set of regs that are going to hold the tags and a valid bit, basically a CAM
                    # | Tag | Valid |
                    tag_valid_cam = self.var("tag_valid_cam", tag_width + 1,
                                               size=max_num_items_wcb, packed=True, explicit_array=True)
                    next_tag_valid_cam = self.var("next_tag_valid_cam", tag_width + 1,
                                               size=max_num_items_wcb, packed=True, explicit_array=True)

                    # FF defined for tag valid cam
                    @always_ff((posedge, "clk"), (negedge, "rst_n"))
                    def tag_valid_cam_ff():
                        if ~self._rst_n:
                            for i in range(max_num_items_wcb):
                                tag_valid_cam[i] = 0
                        elif self._flush:
                            for i in range(max_num_items_wcb):
                                tag_valid_cam[i] = 0
                        elif self._clk_en:
                            for i in range(max_num_items_wcb):
                                tag_valid_cam[i] = next_tag_valid_cam[i]

                    # Get tag bits from last_read_addr
                    tag_bits_last_read_addr = self._last_read_addr[tag_addr_range[0], tag_addr_range[1]]

                    @always_comb
                    # Create the combinational logic for the next_tag_valid_cam signals - should match the linear_wcb_write for the address
                    def next_tag_valid_cam_comb():
                        for i in range(max_num_items_wcb):
                            next_tag_valid_cam[i] = tag_valid_cam[i]
                            if self._data_being_written & (self._linear_wcb_write == i):
                                next_tag_valid_cam[i] = kts.concat(kts.const(1, 1), tag_bits_last_read_addr)
                            elif self._pop_wcb & (self._linear_wcb_read == i):
                                next_tag_valid_cam[i] = kts.concat(kts.const(0, 1), tag_valid_cam[i][tag_valid_cam[i].width - 2, 0])
                        # if self._data_being_written:
                        #     next_tag_valid_cam[self._linear_wcb_write] = kts.concat(kts.const(1, 1), tag_bits_last_read_addr)

                    # Invalidate

                    # Now, need to search the tag_valid_cam for the entry that matches the tag from the output of the tag queue
                    done_tmp = self.var("done_tmp_match_cam", 1)
                    match_addr = self.var("match_addr", kts.clog2(max_num_items_wcb))
                    match_addr_valid = self.var("match_addr_valid", 1)

                    @always_comb
                    def tag_valid_cam_search():
                        done_tmp = 0
                        match_addr = 0
                        match_addr_valid = 0
                        for i in range(max_num_items_wcb):
                            if (tag_valid_cam[i][tag_width - 1, 0] == self._addr_q_tag_out) and ~done_tmp:
                                done_tmp = 1
                                match_addr = i
                                match_addr_valid = tag_valid_cam[i][tag_width]
                    self.add_code(tag_valid_cam_search)

                    self.add_code(tag_valid_cam_ff)
                    self.add_code(next_tag_valid_cam_comb)

                    wp_intf = self._piso_strg_mp_in.get_port_intf()
                    rp_intf = self._piso_strg_mp_out.get_port_intf()

                    # Wire in the inputs and outputs...
                    self.wire(self._pick_input_data, wp_intf['write_data'])
                    self.wire(self._data_being_written, wp_intf['write_en'])
                    self.wire(self._linear_wcb_write, wp_intf['addr'])

                    self.wire(rp_intf['read_data'], self._data_out_lcl)
                    self.wire(self._valid_out_lcl, rp_intf['read_en'])
                    # Also handle address
                    self.wire(kts.concat(match_addr, self._addr_q_sub_addr_out), rp_intf['addr'])
                    ###############################################################
                    ###############################################################
                    ###############################################################

                    q_in = kts.concat(self._addr_q_sub_addr_in, self._addr_q_pop_wcb_in, self._addr_q_tag_in)
                    q_out = kts.concat(self._addr_q_sub_addr_out, self._addr_q_pop_wcb_out, self._addr_q_tag_out)

                    # Need a sub address FIFO
                    # Need a pop signal FIFO (1 bit)
                    # Lastly need storage for the tag
                    reg_fifo = RegFIFO(data_width=addr_q_width + pop_q_width + tag_width, width_mult=1, depth=8)
                    self.add_child(f"reg_fifo_addr_q_pop_q_tag_q",
                                    reg_fifo,
                                    clk=self._clk,
                                    rst_n=self._rst_n,
                                    clk_en=self._clk_en,
                                    flush=self._flush,
                                    push=self._push_addr_q,
                                    pop=self._pop_addr_q,
                                    data_in=q_in,
                                    data_out=q_out)
                    # The local valid is if there are entries in the queue
                    # Also need to deal with startup - make sure there are items in wcb as well
                    self.wire(self._valid_out_lcl, ~reg_fifo.ports.empty & (self._num_items_wcb > 0) & match_addr_valid)

                    if self.opt_timing:

                        # This case doesn't allow the pop to be considered, helping with timing.

                        @always_comb
                        def data_being_written_comb():
                            self._data_being_written = 0
                            # Try only letting it write if the buffer is not full...
                            if self._data_on_bus and (self._num_items_wcb < 2):
                                self._data_being_written = 1
                        self.add_code(data_being_written_comb)

                    else:

                        @always_comb
                        def data_being_written_comb():
                            self._data_being_written = 0
                            # Try only letting it write if the buffer is not full...
                            if self._data_on_bus and (self._pop_wcb | (self._num_items_wcb < 2)):
                                self._data_being_written = 1
                        self.add_code(data_being_written_comb)

                    @always_comb
                    def addr_q_in_comb():

                        # Push the addr queue if data
                        # self._push_addr_q = self._data_being_written | (self._last_read_addr[addr_bits_range[0], addr_bits_range[1]] != self._full_addr_in[addr_bits_range[0], addr_bits_range[1]])
                        self._push_addr_q = 0
                        # If it hasn't been read and we are reading, we should push this address...
                        if ~self._already_read:
                            # If we are making the read...
                            # self._push_addr_q = self._read_memory_out
                            self._push_addr_q = self._read_memory_out & self._grant
                        # If the upper portion of the address is different...
                        elif (self._last_read_addr[addr_bits_range[0], addr_bits_range[1]] != self._full_addr_in[addr_bits_range[0], addr_bits_range[1]]) & (~reg_fifo.ports.full):
                            # self._push_addr_q = self._read_memory_out
                            # This implies a read memory out
                            # self._push_addr_q = self._read_memory_out & ~self._finished
                            self._push_addr_q = self._read_memory_out & ~self._finished & self._grant
                        # If the upper portion is the same, we can push if there is room in the queue
                        elif (~reg_fifo.ports.full) & self._sg_step_in:
                            # self._push_addr_q = 1
                            # Only push to addr queue if not finished...
                            self._push_addr_q = ~self._finished

                        # Easy - just take the sub address from the full address
                        # self._addr_q_sub_addr_in = self._last_read_addr[sub_addr_range[0], sub_addr_range[1]]
                        self._addr_q_sub_addr_in = self._full_addr_in[sub_addr_range[0], sub_addr_range[1]]

                        # We only push a pop in when there is a read to a new address
                        # self._addr_q_pop_wcb_in = self._read_memory_out
                        self._addr_q_pop_wcb_in = self._read_memory_out & self._grant
                        # The tag is just another set of bits
                        # self._addr_q_tag_in = self._last_read_addr[tag_addr_range[0], tag_addr_range[1]]
                        self._addr_q_tag_in = self._full_addr_in[tag_addr_range[0], tag_addr_range[1]]

                    self.add_code(addr_q_in_comb)

                    @always_comb
                    def output_port_comb():

                        self._read_memory_out = 0
                        # Only time we should make a read is if the input step is high, and there's never been a read, or the read address is new, plus there is
                        # either room on the current bus or the current bus is being written
                        # if self._sg_step_in & (~self._already_read | ((self._last_read_addr[addr_bits_range[0], addr_bits_range[1]] != self._full_addr_in[addr_bits_range[0], addr_bits_range[1]]) & (~self._data_on_bus | self._data_being_written))):
                        if ~self._finished & self._sg_step_in & (~self._already_read | ((self._last_read_addr[addr_bits_range[0], addr_bits_range[1]] != self._full_addr_in[addr_bits_range[0], addr_bits_range[1]]) & (~self._data_on_bus | self._data_being_written))):
                            self._read_memory_out = 1

                        # self._sg_step_out = 0
                        # This signal is actually just the push address queue signal
                        self._sg_step_out = self._push_addr_q

                        # We should pop the address queue if there are items in it and the downstream is ready
                        # and the match_addr_valid is 1 - to make sure we are not reading too early.
                        self._pop_addr_q = 0
                        self._pop_wcb = 0
                        if self._valid_out_lcl and ub_interface['ready'] and match_addr_valid:
                            self._pop_addr_q = 1
                            # We should only pop the wcb if there is pop and 2 items
                            if self._addr_q_pop_wcb_out & (self._num_items_wcb == 2):
                                self._pop_wcb = 1

                        # Calculate next if there is data on sram
                        self._next_data_on_bus = self._data_on_bus
                        # This becomes true any time we read...
                        if self._read_memory_out & self._grant:
                            self._next_data_on_bus = 1
                        # It only becomes false in the case that we push to the wcb
                        elif self._data_being_written:
                            self._next_data_on_bus = 0

                        self._next_num_items_wcb = self._num_items_wcb
                        # If we are writing the wcb and not popping, increment the number of items
                        if (self._data_being_written) and (~self._pop_wcb):
                            self._next_num_items_wcb = self._num_items_wcb + 1
                        # If we are popping the wcb, decrement the number of items
                        elif (self._pop_wcb) and (~self._data_being_written):
                            self._next_num_items_wcb = self._num_items_wcb - 1

                    self.add_code(output_port_comb)

                    # Now lift everything's config space up
                    self.config_space_fixed = True
                    self._assemble_cfg_memory_input()

                    return

                # Need to create an rv network here...
                if self._runtime == Runtime.DYNAMIC:
                    self._rv_comp_nw = RVComparisonNetwork(name=f"rv_comp_nw_{self.name}")

                assert self.dimensionality is not None
                assert external_id is not None
                # For the write port with a wide fetch, we need
                # storage and memory ports for SIPO, ID, AG, SG (original size + 1) on the front
                # Capacity of the storage will be w.r.t. external data width2
                if self._ext_data_width > self._int_data_width:
                    storage_vec_cap = self._ext_data_width * self._vec_capacity // 8
                else:
                    storage_vec_cap = self._int_data_width * self._vec_capacity // 8
                self._piso_strg = SingleBankStorage(capacity=storage_vec_cap)
                self._piso_strg_mp_in = MemoryPort(data_width=self._int_data_width, mptype=MemoryPortType.W)
                self._piso_strg_mp_out = MemoryPort(data_width=self._ext_data_width, mptype=MemoryPortType.R, delay=0, active_read=False)

                memports = [self._piso_strg_mp_in, self._piso_strg_mp_out]

                self._piso_strg.gen_hardware(memory_ports=memports)
                self._piso_strg_mp_in.gen_hardware(storage_node=self._piso_strg)
                self._piso_strg_mp_out.gen_hardware(storage_node=self._piso_strg)

                strg_intfs = self._piso_strg.get_memport_intfs()

                self.add_child('vec_storage', self._piso_strg, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                self.add_child('vec_storage_mp_in', self._piso_strg_mp_in, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                self.add_child('vec_storage_mp_out', self._piso_strg_mp_out, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)

                connect_memoryport_storage(self, mptype=self._piso_strg_mp_in.get_type(),
                                           memport_intf=self._piso_strg_mp_in.get_storage_intf(),
                                           strg_intf=strg_intfs[0])
                connect_memoryport_storage(self, mptype=self._piso_strg_mp_out.get_type(),
                                           memport_intf=self._piso_strg_mp_out.get_storage_intf(),
                                           strg_intf=strg_intfs[1])
                # Generate the storage and connect the memory ports

                # Now that we have built the storage and MemoryPort interfaces, we can build the extra controllers and wire them
                # to the memory ports
                # Input side stuff
                self._id_piso_out = IterationDomain(dimensionality=self.dimensionality)
                # self._sg_piso_out = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16)
                self._ag_piso_out = AddressGenerator(dimensionality=self.dimensionality)

                if self._runtime == Runtime.DYNAMIC:
                    self._sg_piso_in = ReadyValidScheduleGenerator(dimensionality=self.dimensionality)
                    self._sg_piso_out = ReadyValidScheduleGenerator(dimensionality=self.dimensionality)
                else:
                    self._sg_piso_out = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16)
                # Output side stuff - don't actually need the ID, SG, can just use the ID, SG from the original spec which will be built to
                # drive the Storage element anyway. Just need to accept a step/enable signal in from the outside anyway
                # self._id_int = IterationDomain(dimensionality=self.dimensionality)
                # self._sg_int = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16)

                # id_dims = port_id.get_dimensionality()
                # memports_ = self.get_memory_ports(port=port)
                # Port needs to know about the dimensionality in case of a vectorized port to
                # build the proper hardware within the port
                # port.gen_hardware(dimensionality=id_dims)
                self._id_piso_out.gen_hardware()
                self._sg_piso_out.gen_hardware(self._id_piso_out)
                self._ag_piso_out.gen_hardware(memports=[self._piso_strg_mp_out], id=self._id_piso_out)

                # Register all sgs with the rv comp network
                if self._runtime == Runtime.DYNAMIC:
                    self._sg_piso_in.gen_hardware(external_id, num_comparisons=1)
                    self.add_child('port_sg_piso_in', self._sg_piso_in,
                                   clk=self._clk,
                                   rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                    self._rv_comp_nw.add_reader_writer(direction=Direction.IN, sg=self._sg_piso_in)
                    self._rv_comp_nw.add_reader_writer(direction=Direction.OUT, sg=self._sg_piso_out)

                    # Also need ID for some reason
                    self._id_piso_in = IterationDomain(dimensionality=self.dimensionality)
                    self._id_piso_in.gen_hardware()
                    self.add_child('port_id_piso_in', self._id_piso_in,
                                   clk=self._clk,
                                   rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)

                # Connect the ag/sg/id together
                self.add_child(f"port_id_piso_out", self._id_piso_out,
                               clk=self._clk,
                               rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                self.add_child(f"port_ag_piso_out", self._ag_piso_out,
                               clk=self._clk,
                               rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                self.add_child(f"port_sg_piso_out", self._sg_piso_out,
                               clk=self._clk,
                               rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)

                # Hook up the ID, SG, AG that are generated internal to the Port
                self.wire(self._id_piso_out.ports.mux_sel, self._ag_piso_out.ports.mux_sel)
                self.wire(self._id_piso_out.ports.iterators, self._ag_piso_out.ports.iterators)
                self.wire(self._id_piso_out.ports.restart, self._ag_piso_out.ports.restart)
                self.wire(self._id_piso_out.ports.finished, self._ag_piso_out.ports.finished)

                self.wire(self._id_piso_out.ports.mux_sel, self._sg_piso_out.ports.mux_sel)
                self.wire(self._id_piso_out.ports.iterators, self._sg_piso_out.ports.iterators)
                self.wire(self._id_piso_out.ports.restart, self._sg_piso_out.ports.restart)
                self.wire(self._id_piso_out.ports.finished, self._sg_piso_out.ports.finished)

                # TODO: Qualify step with the valids + such - can just have them default to 1's in static mode instead
                # of having separate logic blocks every time this happens
                if self._runtime == Runtime.DYNAMIC:
                    # Only step if local step is high and the incoming ready is high
                    self.wire(self._sg_piso_out.ports.step & self._ub_intf['ready'], self._ag_piso_out.ports.step)
                    self.wire(self._sg_piso_out.ports.step & self._ub_intf['ready'], self._id_piso_out.ports.step)
                else:
                    self.wire(self._sg_piso_out.ports.step, self._ag_piso_out.ports.step)
                    self.wire(self._sg_piso_out.ports.step, self._id_piso_out.ports.step)

                # Send through extents if RV
                if self._runtime == Runtime.DYNAMIC:
                    self.wire(self._id_piso_out.ports.extents_out, self._sg_piso_out.ports.extents)

                # Hook up the internal stuff to the memoryport internally
                assembled_port = {}
                assembled_port['data'] = self._ub_intf['data']
                assembled_port['addr'] = self._ag_piso_out.get_address()
                # assembled_port['en'] = self._sg_piso_out.get_step()

                # The data and addr to the SIPO can just be directly wired, but we need to qualify the write enable
                # with the SG's step and input valid
                if self._runtime == Runtime.DYNAMIC:
                    # Can also hook up the ub ready now - it's just the step since there's no arbitration on the MP
                    assembled_port['en'] = self._sg_piso_out.get_step() & self._ub_intf['ready']
                    self.wire(self._ub_intf['valid'], self._sg_piso_out.get_step())
                else:
                    assembled_port['en'] = self._sg_piso_out.get_step()
                    self.wire(self._ub_intf['valid'], self._sg_piso_out.get_step())

                # Now hook up the input and output ports
                piso_out_intf = self._piso_strg_mp_out.get_port_intf()
                print(piso_out_intf)
                self.wire(assembled_port['data'], piso_out_intf['read_data'])
                self.wire(assembled_port['addr'], piso_out_intf['addr'])
                self.wire(assembled_port['en'], piso_out_intf['read_en'])

                # Need to add the AG ports to the port interface to get connected later
                self._ag_piso_in = AddressGenerator(dimensionality=self.dimensionality)
                self._ag_piso_in.gen_hardware(memports=[self._piso_strg_mp_in], id=external_id)
                self._internal_ag_intf = {}

                self.add_child('ag_piso_in', self._ag_piso_in,
                               clk=self._clk,
                               rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                # step_tmp = self.input(f"port_vec_internal_step", 1)
                # self._internal_step['mux_sel'] = self.input(f"port_vec_internal_mux_sel", 1)
                # self._internal_step['iterators'] = self.input(f"port_vec_internal_dim_ctrs", 1)

                # self._internal_ag_intf['step'] = self.input(f"port_piso_in_step", 1)

                # self._internal_ag_intf['restart'] = self.rvinput(name=f"port_piso_in_restart", width=1, packed=True)
                # self._internal_ag_intf['mux_sel'] = self.rvinput(name=f"port_piso_in_mux_sel", width=self._ag_piso_in.ports.mux_sel.width, packed=True)
                # self._internal_ag_intf['iterators'] = self.rvinput(name=f"port_piso_in_dim_ctrs2", width=self._ag_piso_in.ports.iterators.width,
                #                                                    size=self.dimensionality, explicit_array=True, packed=True)

                # self._internal_ag_intf['restart'] = self.input(f"port_piso_in_restart", 1)
                # self._internal_ag_intf['mux_sel'] = self.input(f"port_piso_in_mux_sel", self._ag_piso_in.ports.mux_sel.width)
                # self._internal_ag_intf['iterators'] = self.input(f"port_piso_in_dim_ctrs2", self._ag_piso_in.ports.iterators.width,
                #                                                  size=self.dimensionality, explicit_array=True, packed=True)
                # self.wire(self._internal_ag_intf['step'], self._ag_piso_in.ports.step)
                # self.wire(self._internal_ag_intf['restart'], self._ag_piso_in.ports.restart)
                # self.wire(self._internal_ag_intf['mux_sel'], self._ag_piso_in.ports.mux_sel)
                # self.wire(self._internal_ag_intf['iterators'], self._ag_piso_in.ports.iterators)

                # # Hook up the SG block in the case of dynamic runtime
                # self.wire(self._internal_ag_intf['restart'].get_port(), self._ag_piso_in.ports.restart)
                # self.wire(self._internal_ag_intf['mux_sel'].get_port(), self._ag_piso_in.ports.mux_sel)
                # self.wire(self._internal_ag_intf['iterators'].get_port(), self._ag_piso_in.ports.iterators)
                # Only step the internal AG if the input is valid and we are asserting ready
                # self.wire(self._mp_intf['valid'] & self._mp_intf['ready'], self._ag_piso_in.ports.step)

                if self._runtime == Runtime.DYNAMIC:
                    # self._internal_ag_intf['extents'] = self.input(name="port_piso_in_extents", width=external_id.get_extent_width(),
                    #                                                size=external_id.get_dimensionality(), packed=True, explicit_array=True)
                    # self.wire(self._internal_ag_intf['extents'], self._sg_piso_in.ports.extents)
                    # # self.wire(self._internal_ag_intf['extents'].get_ready(), self._mp_intf['ready'] & self._mp_intf['valid'])
                    # self.wire(self._internal_ag_intf['iterators'].get_port(), self._sg_piso_in.ports.iterators)
                    # self.wire(self._internal_ag_intf['iterators'].get_ready(), self._mp_intf['ready'] & self._mp_intf['valid'])
                    # self.wire(self._internal_ag_intf['mux_sel'].get_port(), self._sg_piso_in.ports.mux_sel)
                    # self.wire(self._internal_ag_intf['mux_sel'].get_ready(), self._mp_intf['ready'] & self._mp_intf['valid'])
                    # self.wire(self._internal_ag_intf['restart'].get_port(), self._sg_piso_in.ports.restart)
                    # self.wire(self._internal_ag_intf['restart'].get_ready(), self._mp_intf['ready'] & self._mp_intf['valid'])

                    self.wire(self._id_piso_in.ports.iterators, self._ag_piso_in.ports.iterators)
                    self.wire(self._id_piso_in.ports.mux_sel, self._ag_piso_in.ports.mux_sel)
                    self.wire(self._id_piso_in.ports.restart, self._ag_piso_in.ports.restart)
                    self.wire(self._id_piso_in.ports.finished, self._ag_piso_in.ports.finished)

                    self.wire(self._id_piso_in.ports.iterators, self._sg_piso_in.ports.iterators)
                    self.wire(self._id_piso_in.ports.mux_sel, self._sg_piso_in.ports.mux_sel)
                    self.wire(self._id_piso_in.ports.restart, self._sg_piso_in.ports.restart)
                    self.wire(self._id_piso_in.ports.finished, self._sg_piso_in.ports.finished)

                    self.wire(self._id_piso_in.ports.extents_out, self._sg_piso_in.ports.extents)

                    # Wire ID and AG step...
                    self.wire(self._mp_intf['valid'] & self._mp_intf['ready'], self._ag_piso_in.ports.step)
                    self.wire(self._mp_intf['valid'] & self._mp_intf['ready'], self._id_piso_in.ports.step)

                else:

                    self._internal_ag_intf['step'] = self.input(name=f"port_piso_in_step", width=1, packed=True)
                    self._internal_ag_intf['restart'] = self.input(name=f"port_piso_in_restart", width=1, packed=True)
                    # self._internal_ag_intf['finished'] = self.input(name=f"port_piso_in_finished", width=1, packed=True)
                    self._internal_ag_intf['finished'] = kts.const(0, 1)
                    self._internal_ag_intf['mux_sel'] = self.input(name=f"port_piso_in_mux_sel", width=self._ag_piso_in.ports.mux_sel.width, packed=True)
                    self._internal_ag_intf['iterators'] = self.input(name=f"port_piso_in_dim_ctrs2", width=self._ag_piso_in.ports.iterators.width,
                                                                     size=self.dimensionality, explicit_array=True, packed=True)

                    # self.wire(self._internal_ag_intf['iterators'].get_ready(), kts.const(1, 1))
                    # self.wire(self._internal_ag_intf['mux_sel'].get_ready(), kts.const(1, 1))
                    # self.wire(self._internal_ag_intf['restart'].get_ready(), kts.const(1, 1))

                    # Hook up the SG block in the case of dynamic runtime
                    self.wire(self._internal_ag_intf['restart'], self._ag_piso_in.ports.restart)
                    self.wire(self._internal_ag_intf['finished'], self._ag_piso_in.ports.finished)
                    self.wire(self._internal_ag_intf['mux_sel'], self._ag_piso_in.ports.mux_sel)
                    self.wire(self._internal_ag_intf['iterators'], self._ag_piso_in.ports.iterators)
                    self.wire(self._mp_intf['valid'] & self._mp_intf['ready'], self._ag_piso_in.ports.step)
                    # self.wire(self._internal_ag_intf['restart'].get_port(), self._ag_piso_in.ports.restart)
                    # self.wire(self._internal_ag_intf['mux_sel'].get_port(), self._ag_piso_in.ports.mux_sel)
                    # self.wire(self._internal_ag_intf['iterators'].get_port(), self._ag_piso_in.ports.iterators)
                    # self.wire(self._mp_intf['valid'] & self._mp_intf['ready'], self._ag_piso_in.ports.step)
                    # self.wire(self._mp_intf['valid'] & self._mp_intf['ready'], self._ag_piso_in.ports.step)
                    # self.wire(self._internal_ag_intf['step'] & self._mp_intf['valid'] & self._mp_intf['ready'], self._ag_piso_in.ports.step)
                    # self.wire(self._internal_ag_intf['step'], self._ag_piso_in.ports.step)

                # TODO: Actually need to delay everything...

                # Wire up the external stuff to the PISO
                assembled_port = {}
                assembled_port['data'] = self._mp_intf['data']
                assembled_port['addr'] = self._ag_piso_in.get_address()
                # assembled_port['en'] = self._internal_ag_intf['step']

                # The data and addr out of the SIPO can just be directly wired, but we need to qualify the read enable
                # with the SG's step and output ready
                if self._runtime == Runtime.DYNAMIC:
                    assembled_port['en'] = self._mp_intf['valid'] & self._sg_piso_in.get_step()
                    # assembled_port['en'] = self._internal_ag_intf['step'] & self._mp_intf['valid'] & self._sg_piso_in.get_step()
                    # Valid is the same as en
                    self.wire(self._mp_intf['ready'], self._sg_piso_in.get_step())
                    # self.wire(self._mp_intf['ready'], self._internal_ag_intf['step'] & self._mp_intf['valid'] & self._sg_piso_in.get_step())
                    # self.wire(self._mp_intf['ready'], self._internal_ag_intf['step'] & self._mp_intf['valid'] & self._sg_piso_in.get_step())
                else:
                    # assembled_port['en'] = self._sg_sipo_in.get_step()
                    assembled_port['en'] = self._internal_ag_intf['step']
                    # assembled_port['en'] = self._mp_intf['valid'] & self._mp_intf['ready']
                    self.wire(self._mp_intf['ready'], self._internal_ag_intf['step'])

                piso_in_intf = self._piso_strg_mp_in.get_port_intf()
                print(piso_in_intf)
                self.wire(assembled_port['data'], piso_in_intf['write_data'])
                self.wire(assembled_port['addr'], piso_in_intf['addr'])
                self.wire(assembled_port['en'], piso_in_intf['write_en'])

                # Now lift the config spaces
                all_to_lift = [self._piso_strg, self._piso_strg_mp_in, self._piso_strg_mp_out,
                               self._id_piso_out, self._sg_piso_out, self._ag_piso_out, self._ag_piso_in]

                # Instantiate and hook up local rv comp network
                if self._runtime == Runtime.DYNAMIC:
                    self._rv_comp_nw.gen_hardware()
                    self.add_child('rv_comp_network_piso', self._rv_comp_nw,
                                   clk=self._clk,
                                   rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
                    rv_comp_conns = self._rv_comp_nw.get_connections()
                    for conn_tuple in rv_comp_conns:
                        p1, p2 = conn_tuple
                        self.wire(p1, p2)

        else:
            raise NotImplementedError

        # Now lift everything's config space up
        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def gen_bitstream(self, vec_in=None, vec_out=None, vec_constraints=None, id_map=None):

        self.clear_configuration()

        all_bs = []

        if self.filter:
            assert id_map is not None, "Need to pass in extents for the filtered version..."
            print("Configuring port...")
            # Need to configure the filter
            # Create 1 by 1 address map for filter
            # Just need extents and dimensionality....
            filter_sg_extents = id_map["extents"]
            filter_sg_dimensionality = id_map["dimensionality"]
            filter_sg_schedule_map = {"offset": 0}
            filter_sg_strides = []

            # Set Defaults
            new_mult = 1
            for i in range(filter_sg_dimensionality):
                filter_sg_strides.append(new_mult)
                new_mult = new_mult * filter_sg_extents[i]
            filter_sg_schedule_map['strides'] = filter_sg_strides

            # Override if the map attributes exist in the map
            if 'offset' in id_map:
                print("Rewrote offset...")
                filter_sg_schedule_map['offset'] = id_map['offset'][0]

            if 'strides' in id_map:
                print("Rewrote strides...")
                filter_sg_schedule_map['strides'] = id_map['strides']

            # Now get the filter bitstream
            filter_bs = self.filter_sg.gen_bitstream(schedule_map=filter_sg_schedule_map,
                                                        extents=filter_sg_extents,
                                                        dimensionality=filter_sg_dimensionality)
            filter_bs = self._add_base_to_cfg_space(filter_bs, self.child_cfg_bases[self.filter_sg])
            all_bs.append(filter_bs)

        # Only generate vec bitstream if it's vecced
        if vec_in is not None or vec_out is not None:

            assert vec_in is not None and vec_out is not None and self._fw != 1

            if self.get_direction() == Direction.IN:

                if self.opt_rv:
                    pass
                else:
                    vec_in_addr_map = vec_in['address']
                    vec_in_sched_map = vec_in['schedule']
                    internal_id_bs = self._id_sipo_in.gen_bitstream(dimensionality=vec_in['dimensionality'],
                                                                    extents=vec_in['extents'],
                                                                    rv=self._runtime == Runtime.DYNAMIC)
                    internal_ag_bs = self._ag_sipo_in.gen_bitstream(address_map=vec_in_addr_map,
                                                                    extents=vec_in['extents'],
                                                                    dimensionality=vec_in['dimensionality'])
                    internal_sg_bs = self._sg_sipo_in.gen_bitstream(schedule_map=vec_in_sched_map,
                                                                    extents=vec_in['extents'],
                                                                    dimensionality=vec_in['dimensionality'])

                    internal_id_bs = self._add_base_to_cfg_space(internal_id_bs, self.child_cfg_bases[self._id_sipo_in])
                    internal_ag_bs = self._add_base_to_cfg_space(internal_ag_bs, self.child_cfg_bases[self._ag_sipo_in])
                    internal_sg_bs = self._add_base_to_cfg_space(internal_sg_bs, self.child_cfg_bases[self._sg_sipo_in])
                    # add on the respective base
                    # Now get the output part
                    vec_out_addr_map = vec_out['address']
                    # vec_out_sched_map = vec_out['schedule']
                    external_ag_bs = self._ag_sipo_out.gen_bitstream(address_map=vec_out_addr_map,
                                                                    extents=vec_out['extents'],
                                                                    dimensionality=vec_out['dimensionality'])
                    external_ag_bs = self._add_base_to_cfg_space(external_ag_bs, self.child_cfg_bases[self._ag_sipo_out])

                    all_bs = [internal_id_bs, internal_ag_bs, internal_sg_bs, external_ag_bs]

                    # Need to configure the rv sched generator at the output of the sipo
                    if self._runtime == Runtime.DYNAMIC:
                        sg_sipo_out_bs = self._sg_sipo_out.gen_bitstream()
                        sg_sipo_out_bs = self._add_base_to_cfg_space(sg_sipo_out_bs, self.child_cfg_bases[self._sg_sipo_out])
                        all_bs.append(sg_sipo_out_bs)

            elif self.get_direction() == Direction.OUT:

                if self.opt_rv:
                    pass

                else:
                    vec_out_addr_map = vec_out['address']
                    vec_out_sched_map = vec_out['schedule']
                    internal_id_bs = self._id_piso_out.gen_bitstream(dimensionality=vec_out['dimensionality'],
                                                                    extents=vec_out['extents'],
                                                                    rv=self._runtime == Runtime.DYNAMIC)
                    internal_ag_bs = self._ag_piso_out.gen_bitstream(address_map=vec_out_addr_map,
                                                                    extents=vec_out['extents'],
                                                                    dimensionality=vec_out['dimensionality'])
                    internal_sg_bs = self._sg_piso_out.gen_bitstream(schedule_map=vec_out_sched_map,
                                                                    extents=vec_out['extents'],
                                                                    dimensionality=vec_out['dimensionality'])

                    internal_id_bs = self._add_base_to_cfg_space(internal_id_bs, self.child_cfg_bases[self._id_piso_out])
                    internal_ag_bs = self._add_base_to_cfg_space(internal_ag_bs, self.child_cfg_bases[self._ag_piso_out])
                    internal_sg_bs = self._add_base_to_cfg_space(internal_sg_bs, self.child_cfg_bases[self._sg_piso_out])

                    # Now get the output part
                    vec_out_addr_map = vec_in['address']
                    external_ag_bs = self._ag_piso_in.gen_bitstream(address_map=vec_out_addr_map,
                                                                    extents=vec_in['extents'],
                                                                    dimensionality=vec_in['dimensionality'])
                    external_ag_bs = self._add_base_to_cfg_space(external_ag_bs, self.child_cfg_bases[self._ag_piso_in])
                    all_bs = [internal_id_bs, internal_ag_bs, internal_sg_bs, external_ag_bs]

                    # Need to configure the rv sched generator at the output of the sipo
                    if self._runtime == Runtime.DYNAMIC:
                        sg_piso_in_bs = self._sg_piso_in.gen_bitstream()
                        sg_piso_in_bs = self._add_base_to_cfg_space(sg_piso_in_bs, self.child_cfg_bases[self._sg_piso_in])
                        all_bs.append(sg_piso_in_bs)

                        id_piso_in_bs = self._id_piso_in.gen_bitstream(dimensionality=vec_in['dimensionality'],
                                                                    extents=vec_in['extents'],
                                                                    rv=self._runtime == Runtime.DYNAMIC)
                        id_piso_in_bs = self._add_base_to_cfg_space(id_piso_in_bs, self.child_cfg_bases[self._id_piso_in])
                        all_bs.append(id_piso_in_bs)

            else:
                raise NotImplementedError

            # Need to configure the RV network if rv mode
            # if self._runtime == Runtime.DYNAMIC and (self.opt_rv is False or (self.get_direction() == Direction.IN)):
            if self._runtime == Runtime.DYNAMIC and self.opt_rv is False:
                rv_comp_bs = self._rv_comp_nw.gen_bitstream(constraints=vec_constraints)
                rv_comp_bs = self._add_base_to_cfg_space(rv_comp_bs, self.child_cfg_bases[self._rv_comp_nw])
                all_bs.append(rv_comp_bs)

        for cfg_ in all_bs:
            self._add_configuration_manual(config=cfg_)

        return self.get_configuration()

    def get_direction(self):
        return self._direction

    def get_runtime(self):
        return self._runtime

    def get_mp_intf(self):
        return self._mp_intf

    def get_ub_intf(self):
        return self._ub_intf


class ReadPort(Port):

    def __init__(self, data_width=16, runtime=Runtime.STATIC):
        super().__init__(data_width=data_width, runtime=runtime)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        self.clear_configuration()
        return super().gen_bitstream()


class WritePort(Port):

    def __init__(self, data_width=16, direction=Runtime.STATIC):
        super().__init__(data_width=data_width, direction=direction)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        self.clear_configuration()
        return super().gen_bitstream()
