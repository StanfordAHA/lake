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
                 vec_capacity=None, opt_rv=False):
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

    def __str__(self):
        type_str = "Write"
        if self._direction == Direction.OUT:
            type_str = "Read"
        return f"Port: {type_str}"

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
            data_from_ub = self.rvinput(name=f"port_write_data_in", width=self._ext_data_width)
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
                    data_from_pintf = data_from_ub.get_port_interface()
                    data_to_pintf = data_to_memport.get_port_interface()
                    self.wire(data_from_pintf['data'], data_to_pintf['data'])
                    self.wire(data_from_pintf['valid'], data_to_pintf['valid'])
                    self.wire(data_from_pintf['ready'], data_to_pintf['ready'])

            else:

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

                self.add_child('vec_storage', self._sipo_strg, clk=self._clk, rst_n=self._rst_n)
                self.add_child('vec_storage_mp_in', self._sipo_strg_mp_in, clk=self._clk, rst_n=self._rst_n)
                self.add_child('vec_storage_mp_out', self._sipo_strg_mp_out, clk=self._clk, rst_n=self._rst_n)

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
                                   rst_n=self._rst_n)
                    self._rv_comp_nw.add_reader_writer(direction=Direction.IN, sg=self._sg_sipo_in)
                    self._rv_comp_nw.add_reader_writer(direction=Direction.OUT, sg=self._sg_sipo_out)

                self._ag_sipo_in.gen_hardware(memports=[self._sipo_strg_mp_in], id=self._id_sipo_in)

                # Connect the ag/sg/id together
                self.add_child(f"port_id_sipo_in", self._id_sipo_in,
                               clk=self._clk,
                               rst_n=self._rst_n)
                self.add_child(f"port_ag_sipo_in", self._ag_sipo_in,
                               clk=self._clk,
                               rst_n=self._rst_n)
                self.add_child(f"port_sg_sipo_in", self._sg_sipo_in,
                               clk=self._clk,
                               rst_n=self._rst_n)

                self.wire(self._id_sipo_in.ports.mux_sel, self._ag_sipo_in.ports.mux_sel)
                self.wire(self._id_sipo_in.ports.iterators, self._ag_sipo_in.ports.iterators)
                self.wire(self._id_sipo_in.ports.restart, self._ag_sipo_in.ports.restart)

                self.wire(self._id_sipo_in.ports.mux_sel, self._sg_sipo_in.ports.mux_sel)
                self.wire(self._id_sipo_in.ports.iterators, self._sg_sipo_in.ports.iterators)
                self.wire(self._id_sipo_in.ports.restart, self._sg_sipo_in.ports.restart)

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
                               rst_n=self._rst_n)
                # step_tmp = self.input(f"port_vec_internal_step", 1)
                # self._internal_step['mux_sel'] = self.input(f"port_vec_internal_mux_sel", 1)
                # self._internal_step['iterators'] = self.input(f"port_vec_internal_dim_ctrs", 1)

                self._internal_ag_intf['step'] = self.input(f"port_sipo_out_step", 1)
                self._internal_ag_intf['restart'] = self.input(f"port_sipo_out_restart", 1)
                self._internal_ag_intf['mux_sel'] = self.input(f"port_sipo_out_mux_sel", self._ag_sipo_out.ports.mux_sel.width)
                self._internal_ag_intf['iterators'] = self.input(f"port_sipo_out_dim_ctrs", self._ag_sipo_out.ports.iterators.width,
                                                                 size=self.dimensionality, packed=True, explicit_array=True)
                # self.wire(self._internal_ag_intf['step'], self._ag_sipo_out.ports.step)
                self.wire(self._internal_ag_intf['mux_sel'], self._ag_sipo_out.ports.mux_sel)
                self.wire(self._internal_ag_intf['iterators'], self._ag_sipo_out.ports.iterators)
                self.wire(self._internal_ag_intf['restart'], self._ag_sipo_out.ports.restart)
                if self._runtime == Runtime.DYNAMIC:
                    self._internal_ag_intf['extents'] = self.input("port_sipo_out_extents", external_id.get_extent_width(),
                                                                   size=external_id.get_dimensionality(), packed=True, explicit_array=True)
                    self.wire(self._internal_ag_intf['extents'], self._sg_sipo_out.ports.extents)
                    self.wire(self._internal_ag_intf['iterators'], self._sg_sipo_out.ports.iterators)
                    self.wire(self._internal_ag_intf['mux_sel'], self._sg_sipo_out.ports.mux_sel)
                    self.wire(self._internal_ag_intf['restart'], self._sg_sipo_out.ports.restart)
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
                                   rst_n=self._rst_n)
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
                    self._sg_step_in = self.input("sg_step_in", width=1)
                    # This will be used to send out to the AG/ID, etc.
                    # This is also telling us if we are making a memory read this cycle
                    self._sg_step_out = self.output("sg_step_out", width=1)
                    self._data_being_written = self.var("data_being_written", 1)
                    # self._inc_linear_wcb_write = self.var("inc_linear_wcb_write", 1)

                    # self._last_read_addr = self.var("last_read_addr", 1)
                    # self._read_last_cycle = self.var("read_last_cycle", 1)
                    # self._already_read = self.var("already_read", 1)
                    addr_bits_range = [self._full_addr_in.width - 1, kts.clog2(self._fw)]

                    self._already_read = sticky_flag(self, self._sg_step_out, name="already_read", seq_only=True)
                    self._read_last_cycle = register(self, self._sg_step_out, enable=kts.const(1, 1), name="read_last_cycle")
                    self._last_read_addr = register(self, self._full_addr_in, enable=self._sg_step_out, name="last_read_addr")

                    # Define all the relevant signals/vars
                    addr_q_width = kts.clog2(self.get_fw())
                    pop_q_width = 1
                    # tag_width = kts.clog2(self._vec_capacity // 8)
                    tag_width = 1
                    print(f"Tag width: {tag_width}")
                    # exit()

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
                        else:
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
                        else:
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
                    self._data_from_mp_reg = register(self, mp_intf['data'], enable=self._register_data_from_mp, name="data_from_mp_reg")
                    # If we read from SRAM last cycle, can use the data from the SRAM, otherwise we need to grab it from the local register
                    self._pick_input_data = kts.ternary(self._read_last_cycle, self._data_from_mp_reg, mp_intf['data'])

                    # Port interface's valid is the local valid
                    # The data is from the WCB
                    ub_interface = data_to_ub.get_port_interface()
                    self.wire(self._valid_out_lcl, ub_interface['valid'])
                    self.wire(self._data_out_lcl, ub_interface['data'])

                    # Linear write address to the WCB
                    # self._linear_wcb_write = self.var("linear_wcb_write", kts.clog2(self._vec_capacity))
                    self._linear_wcb_write = add_counter(self, "linear_wcb_write", bitwidth=kts.clog2(self._vec_capacity),
                                                         increment=self._data_being_written)

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

                    self.add_child('vec_storage', self._piso_strg, clk=self._clk, rst_n=self._rst_n)
                    self.add_child('vec_storage_mp_in', self._piso_strg_mp_in, clk=self._clk, rst_n=self._rst_n)
                    self.add_child('vec_storage_mp_out', self._piso_strg_mp_out, clk=self._clk, rst_n=self._rst_n)

                    connect_memoryport_storage(self, mptype=self._piso_strg_mp_in.get_type(),
                                               memport_intf=self._piso_strg_mp_in.get_storage_intf(),
                                               strg_intf=strg_intfs[0])
                    connect_memoryport_storage(self, mptype=self._piso_strg_mp_out.get_type(),
                                               memport_intf=self._piso_strg_mp_out.get_storage_intf(),
                                               strg_intf=strg_intfs[1])

                    wp_intf = self._piso_strg_mp_in.get_port_intf()
                    rp_intf = self._piso_strg_mp_out.get_port_intf()

                    # Wire in the inputs and outputs...
                    self.wire(self._pick_input_data, wp_intf['write_data'])
                    self.wire(self._data_being_written, wp_intf['write_en'])
                    self.wire(self._linear_wcb_write, wp_intf['addr'])

                    self.wire(rp_intf['read_data'], self._data_out_lcl)
                    self.wire(self._valid_out_lcl, rp_intf['read_en'])
                    # Also handle address
                    self.wire(kts.concat(kts.const(0, 1), self._addr_q_sub_addr_out), rp_intf['addr'])

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
                                    # clk_en=self._clk_en,
                                    clk_en=kts.const(1, 1),
                                    push=self._push_addr_q,
                                    pop=self._pop_addr_q,
                                    data_in=q_in,
                                    data_out=q_out)
                    # The local valid is if there are entries in the queue
                    self.wire(self._valid_out_lcl, ~reg_fifo.ports.empty)

                    @always_comb
                    def data_being_written_comb():
                        self._data_being_written = 0
                        if self._data_on_bus and (self._pop_wcb | (self._num_items_wcb < 2)):
                            self._data_being_written = 1
                    self.add_code(data_being_written_comb)

                    sub_addr_range = [kts.clog2(self._fw) - 1, 0]
                    tag_addr_range = [kts.clog2(self._fw) + tag_width - 1, kts.clog2(self._fw)]

                    @always_comb
                    def addr_q_in_comb():

                        # Push the addr queue if data
                        self._push_addr_q = self._data_being_written | (self._last_read_addr[addr_bits_range[0], addr_bits_range[1]] != self._full_addr_in[addr_bits_range[0], addr_bits_range[1]])

                        # Easy - just take the sub address from the full address
                        self._addr_q_sub_addr_in = self._last_read_addr[sub_addr_range[0], sub_addr_range[1]]

                        # We only push a pop in when there is a read to a new address
                        self._addr_q_pop_wcb_in = self._sg_step_out
                        # The tag is just another set of bits
                        self._addr_q_tag_in = self._last_read_addr[tag_addr_range[0], tag_addr_range[1]]

                    self.add_code(addr_q_in_comb)

                    @always_comb
                    def output_port_comb():

                        self._sg_step_out = 0
                        # Only time we should make a read is if the input step is high, and there's never been a read, or the read address is new, plus there is
                        # either room on the current bus or the current bus is being written
                        if self._sg_step_in & (~self._already_read | ((self._last_read_addr[addr_bits_range[0], addr_bits_range[1]] != self._full_addr_in[addr_bits_range[0], addr_bits_range[1]]) & (~self._data_on_bus | self._data_being_written))):
                            self._sg_step_out = 1

                        # We should pop the address queue if there are items in it and the downstream is ready
                        self._pop_addr_q = 0
                        self._pop_wcb = 0
                        if self._valid_out_lcl & ub_interface['ready']:
                            self._pop_addr_q = 1
                            # We should only pop the wcb if there is pop and 2 items
                            if self._addr_q_pop_wcb_out & (self._num_items_wcb == 2):
                                self._pop_wcb = 1

                        # Calculate next if there is data on sram
                        self._next_data_on_bus = self._data_on_bus
                        # This becomes true any time we read...
                        if self._sg_step_out:
                            self._next_data_on_bus = 1
                        # It only becomes false in the case that we push to the wcb
                        elif self._data_being_written:
                            self._next_data_on_bus = 0

                        self._next_num_items_wcb = self._num_items_wcb
                        # If we are writing the wcb and not popping, increment the number of items
                        if self._data_being_written and ~self._pop_wcb:
                            self._next_num_items_wcb = self._num_items_wcb + 1
                        # If we are popping the wcb, decrement the number of items
                        elif ~self._data_being_written and self._pop_wcb:
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

                self.add_child('vec_storage', self._piso_strg, clk=self._clk, rst_n=self._rst_n)
                self.add_child('vec_storage_mp_in', self._piso_strg_mp_in, clk=self._clk, rst_n=self._rst_n)
                self.add_child('vec_storage_mp_out', self._piso_strg_mp_out, clk=self._clk, rst_n=self._rst_n)

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
                                   rst_n=self._rst_n)
                    self._rv_comp_nw.add_reader_writer(direction=Direction.IN, sg=self._sg_piso_in)
                    self._rv_comp_nw.add_reader_writer(direction=Direction.OUT, sg=self._sg_piso_out)

                    # Also need ID for some reason
                    self._id_piso_in = IterationDomain(dimensionality=self.dimensionality)
                    self._id_piso_in.gen_hardware()
                    self.add_child('port_id_piso_in', self._id_piso_in,
                                   clk=self._clk,
                                   rst_n=self._rst_n)

                # Connect the ag/sg/id together
                self.add_child(f"port_id_piso_out", self._id_piso_out,
                               clk=self._clk,
                               rst_n=self._rst_n)
                self.add_child(f"port_ag_piso_out", self._ag_piso_out,
                               clk=self._clk,
                               rst_n=self._rst_n)
                self.add_child(f"port_sg_piso_out", self._sg_piso_out,
                               clk=self._clk,
                               rst_n=self._rst_n)

                # Hook up the ID, SG, AG that are generated internal to the Port
                self.wire(self._id_piso_out.ports.mux_sel, self._ag_piso_out.ports.mux_sel)
                self.wire(self._id_piso_out.ports.iterators, self._ag_piso_out.ports.iterators)
                self.wire(self._id_piso_out.ports.restart, self._ag_piso_out.ports.restart)

                self.wire(self._id_piso_out.ports.mux_sel, self._sg_piso_out.ports.mux_sel)
                self.wire(self._id_piso_out.ports.iterators, self._sg_piso_out.ports.iterators)
                self.wire(self._id_piso_out.ports.restart, self._sg_piso_out.ports.restart)

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
                               rst_n=self._rst_n)
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
                    self.wire(self._id_piso_in.ports.iterators, self._sg_piso_in.ports.iterators)
                    self.wire(self._id_piso_in.ports.mux_sel, self._ag_piso_in.ports.mux_sel)
                    self.wire(self._id_piso_in.ports.mux_sel, self._sg_piso_in.ports.mux_sel)
                    self.wire(self._id_piso_in.ports.restart, self._ag_piso_in.ports.restart)
                    self.wire(self._id_piso_in.ports.restart, self._sg_piso_in.ports.restart)
                    self.wire(self._id_piso_in.ports.extents_out, self._sg_piso_in.ports.extents)

                    # Wire ID and AG step...
                    self.wire(self._mp_intf['valid'] & self._mp_intf['ready'], self._ag_piso_in.ports.step)
                    self.wire(self._mp_intf['valid'] & self._mp_intf['ready'], self._id_piso_in.ports.step)

                else:

                    self._internal_ag_intf['step'] = self.input(name=f"port_piso_in_step", width=1, packed=True)
                    self._internal_ag_intf['restart'] = self.input(name=f"port_piso_in_restart", width=1, packed=True)
                    self._internal_ag_intf['mux_sel'] = self.input(name=f"port_piso_in_mux_sel", width=self._ag_piso_in.ports.mux_sel.width, packed=True)
                    self._internal_ag_intf['iterators'] = self.input(name=f"port_piso_in_dim_ctrs2", width=self._ag_piso_in.ports.iterators.width,
                                                                     size=self.dimensionality, explicit_array=True, packed=True)

                    # self.wire(self._internal_ag_intf['iterators'].get_ready(), kts.const(1, 1))
                    # self.wire(self._internal_ag_intf['mux_sel'].get_ready(), kts.const(1, 1))
                    # self.wire(self._internal_ag_intf['restart'].get_ready(), kts.const(1, 1))

                    # Hook up the SG block in the case of dynamic runtime
                    self.wire(self._internal_ag_intf['restart'], self._ag_piso_in.ports.restart)
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
                                   rst_n=self._rst_n)
                    rv_comp_conns = self._rv_comp_nw.get_connections()
                    for conn_tuple in rv_comp_conns:
                        p1, p2 = conn_tuple
                        self.wire(p1, p2)

        else:
            raise NotImplementedError

        # Now lift everything's config space up
        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def gen_bitstream(self, vec_in=None, vec_out=None, vec_constraints=None):

        all_bs = []

        # Only generate vec bitstream if it's vecced
        if vec_in is not None or vec_out is not None:
            assert vec_in is not None and vec_out is not None and self._fw != 1
            if self.get_direction() == Direction.IN:
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
        return super().gen_bitstream()


class WritePort(Port):

    def __init__(self, data_width=16, direction=Runtime.STATIC):
        super().__init__(data_width=data_width, direction=direction)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()
