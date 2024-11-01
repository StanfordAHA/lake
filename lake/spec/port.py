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
from lake.utils.util import connect_memoryport_storage
import kratos as kts


def print_class_hierarchy(obj):
    print("Class Hierarchy:")
    for cls in obj.__class__.__mro__:
        print(cls.__name__)


class Port(Component):

    def __init__(self, ext_data_width=16, int_data_width=16,
                 runtime=Runtime.STATIC, direction=Direction.IN,
                 vec_capacity=None):
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
        self._color = "#D9E7D6"

    def __str__(self):
        type_str = "IN"
        if self._direction == Direction.OUT:
            type_str = "OUT"
        return f"Port_{self._component_num}_{type_str}"

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

                # Need to create a n rv network here...
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
            if self._runtime == Runtime.DYNAMIC:
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
