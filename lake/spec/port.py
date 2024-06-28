from lake.spec.component import Component
from lake.utils.spec_enum import *
import random as rand
from lake.spec.address_generator import AddressGenerator
from lake.spec.memory_port import MemoryPort
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.storage import SingleBankStorage, Storage
from lake.utils.util import connect_memoryport_storage, lift_config_space


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

        self._clk = self.clock('clk')
        self._rst_n = self.reset('rst_n')

        # Going to always have a ready/valid in

        if dimensionality is not None:
            self.set_dimensionality(dimensionality)

        if self._direction == Direction.IN:

            # This is the hardware for a single fetch width
            # in set
            data_from_ub = self.input(f"port_write_data_in", self._ext_data_width)
            data_to_memport = self.output(f"port_write_data_out", self._int_data_width)
            self._ub_intf['data'] = data_from_ub
            self._mp_intf['data'] = data_to_memport

            if self._fw == 1:
                # wire together
                self.wire(data_from_ub, data_to_memport)

            else:
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

                self.add_child('vec_storage', self._sipo_strg, clk=self._clk)
                self.add_child('vec_storage_mp_in', self._sipo_strg_mp_in)
                self.add_child('vec_storage_mp_out', self._sipo_strg_mp_out)

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
                self._id_ext = IterationDomain(dimensionality=self.dimensionality)
                self._sg_ext = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16)
                self._ag_ext = AddressGenerator(dimensionality=self.dimensionality)
                # Output side stuff - don't actually need the ID, SG, can just use the ID, SG from the original spec which will be built to
                # drive the Storage element anyway. Just need to accept a step/enable signal in from the outside anyway
                # self._id_int = IterationDomain(dimensionality=self.dimensionality)
                # self._sg_int = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16)

                # id_dims = port_id.get_dimensionality()
                # memports_ = self.get_memory_ports(port=port)
                # Port needs to know about the dimensionality in case of a vectorized port to
                # build the proper hardware within the port
                # port.gen_hardware(dimensionality=id_dims)
                self._id_ext.gen_hardware()
                self._sg_ext.gen_hardware(self._id_ext)
                self._ag_ext.gen_hardware(memports=[self._sipo_strg_mp_in], id=self._id_ext)

                # Connect the ag/sg/id together
                self.add_child(f"port_id_ext_in", self._id_ext,
                                        clk=self._clk,
                                        rst_n=self._rst_n)
                self.add_child(f"port_ag_ext_in", self._ag_ext,
                                        clk=self._clk,
                                        rst_n=self._rst_n)
                self.add_child(f"port_sg_ext_in", self._sg_ext,
                                        clk=self._clk,
                                        rst_n=self._rst_n)

                self.wire(self._id_ext.ports.mux_sel, self._ag_ext.ports.mux_sel)
                self.wire(self._id_ext.ports.iterators, self._ag_ext.ports.iterators)

                self.wire(self._id_ext.ports.mux_sel, self._sg_ext.ports.mux_sel)
                self.wire(self._id_ext.ports.iterators, self._sg_ext.ports.iterators)

                self.wire(self._sg_ext.ports.step, self._ag_ext.ports.step)
                self.wire(self._sg_ext.ports.step, self._id_ext.ports.step)

                assembled_port = {}
                assembled_port['data'] = data_from_ub
                assembled_port['addr'] = self._ag_ext.get_address()
                assembled_port['en'] = self._sg_ext.get_step()

                # Now hook up the input and output ports
                in_intf = self._sipo_strg_mp_in.get_port_intf()
                print(in_intf)
                self.wire(assembled_port['data'], in_intf['write_data'])
                self.wire(assembled_port['addr'], in_intf['addr'])
                self.wire(assembled_port['en'], in_intf['write_en'])

                # Need to add the AG ports to the port interface to get connected later
                self._ag_int = AddressGenerator(dimensionality=self.dimensionality)
                self._ag_int.gen_hardware(memports=[self._sipo_strg_mp_out], id=external_id)
                self._internal_ag_intf = {}

                self.add_child('internal_ag_with_ext_intf', self._ag_int, clk=self._clk,
                               rst_n=self._rst_n)
                # step_tmp = self.input(f"port_vec_internal_step", 1)
                # self._internal_step['mux_sel'] = self.input(f"port_vec_internal_mux_sel", 1)
                # self._internal_step['iterators'] = self.input(f"port_vec_internal_dim_ctrs", 1)

                self._internal_ag_intf['step'] = self.input(f"port_vec_internal_step", 1)
                self._internal_ag_intf['mux_sel'] = self.input(f"port_vec_internal_mux_sel", self._ag_int.ports.mux_sel.width)
                self._internal_ag_intf['iterators'] = self.input(f"port_vec_internal_dim_ctrs", self._ag_int.ports.iterators.width,
                                                                 size=self.dimensionality, packed=True, explicit_array=True)
                self.wire(self._internal_ag_intf['step'], self._ag_int.ports.step)
                self.wire(self._internal_ag_intf['mux_sel'], self._ag_int.ports.mux_sel)
                self.wire(self._internal_ag_intf['iterators'], self._ag_int.ports.iterators)

                assembled_port = {}
                assembled_port['data'] = data_to_memport
                assembled_port['addr'] = self._ag_int.get_address()
                assembled_port['en'] = self._internal_ag_intf['step']

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
                               self._id_ext, self._sg_ext, self._ag_ext, self._ag_int]

        elif self._direction == Direction.OUT:

            # in set
            data_from_memport = self.input(f"port_read_data_in", self._int_data_width)
            data_to_ub = self.output(f"port_read_data_out", self._ext_data_width)
            self._ub_intf['data'] = data_to_ub
            self._mp_intf['data'] = data_from_memport

            if self._fw == 1:
                # wire together
                self.wire(data_from_memport, data_to_ub)
            else:

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

                self.add_child('vec_storage', self._piso_strg, clk=self._clk)
                self.add_child('vec_storage_mp_in', self._piso_strg_mp_in)
                self.add_child('vec_storage_mp_out', self._piso_strg_mp_out)

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
                self._id_int = IterationDomain(dimensionality=self.dimensionality)
                self._sg_int = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16)
                self._ag_int = AddressGenerator(dimensionality=self.dimensionality)
                # Output side stuff - don't actually need the ID, SG, can just use the ID, SG from the original spec which will be built to
                # drive the Storage element anyway. Just need to accept a step/enable signal in from the outside anyway
                # self._id_int = IterationDomain(dimensionality=self.dimensionality)
                # self._sg_int = ScheduleGenerator(dimensionality=self.dimensionality, stride_width=16)

                # id_dims = port_id.get_dimensionality()
                # memports_ = self.get_memory_ports(port=port)
                # Port needs to know about the dimensionality in case of a vectorized port to
                # build the proper hardware within the port
                # port.gen_hardware(dimensionality=id_dims)
                self._id_int.gen_hardware()
                self._sg_int.gen_hardware(self._id_int)
                self._ag_int.gen_hardware(memports=[self._piso_strg_mp_in], id=self._id_int)

                # Connect the ag/sg/id together
                self.add_child(f"port_id_int_in", self._id_int,
                                        clk=self._clk,
                                        rst_n=self._rst_n)
                self.add_child(f"port_ag_int_in", self._ag_int,
                                        clk=self._clk,
                                        rst_n=self._rst_n)
                self.add_child(f"port_sg_int_in", self._sg_int,
                                        clk=self._clk,
                                        rst_n=self._rst_n)

                # Hook up the ID, SG, AG that are generated internal to the Port
                self.wire(self._id_int.ports.mux_sel, self._ag_int.ports.mux_sel)
                self.wire(self._id_int.ports.iterators, self._ag_int.ports.iterators)

                self.wire(self._id_int.ports.mux_sel, self._sg_int.ports.mux_sel)
                self.wire(self._id_int.ports.iterators, self._sg_int.ports.iterators)

                self.wire(self._sg_int.ports.step, self._ag_int.ports.step)
                self.wire(self._sg_int.ports.step, self._id_int.ports.step)

                # Hook up the internal stuff to the memoryport internally
                assembled_port = {}
                assembled_port['data'] = data_from_memport
                assembled_port['addr'] = self._ag_int.get_address()
                assembled_port['en'] = self._sg_int.get_step()

                # Now hook up the input and output ports
                in_intf = self._piso_strg_mp_in.get_port_intf()
                print(in_intf)
                self.wire(assembled_port['data'], in_intf['write_data'])
                self.wire(assembled_port['addr'], in_intf['addr'])
                self.wire(assembled_port['en'], in_intf['write_en'])

                # Need to add the AG ports to the port interface to get connected later
                self._ag_ext = AddressGenerator(dimensionality=self.dimensionality)
                self._ag_ext.gen_hardware(memports=[self._piso_strg_mp_out], id=external_id)
                self._internal_ag_intf = {}

                self.add_child('internal_ag_with_ext_intf', self._ag_ext, clk=self._clk,
                               rst_n=self._rst_n)
                # step_tmp = self.input(f"port_vec_internal_step", 1)
                # self._internal_step['mux_sel'] = self.input(f"port_vec_internal_mux_sel", 1)
                # self._internal_step['iterators'] = self.input(f"port_vec_internal_dim_ctrs", 1)

                self._internal_ag_intf['step'] = self.input(f"port_vec_internal_step", 1)
                self._internal_ag_intf['mux_sel'] = self.input(f"port_vec_internal_mux_sel", self._ag_ext.ports.mux_sel.width)
                self._internal_ag_intf['iterators'] = self.input(f"port_vec_internal_dim_ctrs2", self._ag_ext.ports.iterators.width,
                                                                 size=self.dimensionality, explicit_array=True, packed=True)
                self.wire(self._internal_ag_intf['step'], self._ag_ext.ports.step)
                self.wire(self._internal_ag_intf['mux_sel'], self._ag_ext.ports.mux_sel)
                self.wire(self._internal_ag_intf['iterators'], self._ag_ext.ports.iterators)

                # Wire up the external stuff to the PISO
                assembled_port = {}
                assembled_port['data'] = data_to_ub
                assembled_port['addr'] = self._ag_ext.get_address()
                assembled_port['en'] = self._internal_ag_intf['step']

                out_intf = self._piso_strg_mp_out.get_port_intf()
                print(out_intf)
                self.wire(assembled_port['data'], out_intf['read_data'])
                self.wire(assembled_port['addr'], out_intf['addr'])
                self.wire(assembled_port['en'], out_intf['read_en'])

                # Now lift the config spaces
                all_to_lift = [self._piso_strg, self._piso_strg_mp_in, self._piso_strg_mp_out,
                               self._id_int, self._sg_int, self._ag_int, self._ag_ext]

        else:
            raise NotImplementedError

        # Now lift everything's config space up
        self.child_cfg_bases = {}
        running_config_size = 0
        for component_ in all_to_lift:
            # component_: Component
            lift_config_space(self, component_)
            self.child_cfg_bases[component_] = running_config_size
            running_config_size += component_.get_config_size()
        self.config_space_fixed = True

    def gen_bitstream(self, vec_in=None, vec_out=None):

        all_bs = []

        # Only generate vec bitstream if it's vecced
        if vec_in is not None or vec_out is not None:
            assert vec_in is not None and vec_out is not None and self._fw != 1
            if self.get_direction() == Direction.IN:
                vec_in_addr_map = vec_in['address']
                vec_in_sched_map = vec_in['schedule']
                internal_id_bs = self._id_ext.gen_bitstream(dimensionality=vec_in['dimensionality'],
                                                            extents=vec_in['extents'])
                internal_ag_bs = self._ag_ext.gen_bitstream(address_map=vec_in_addr_map)
                internal_sg_bs = self._sg_ext.gen_bitstream(schedule_map=vec_in_sched_map)

                internal_id_bs = self._add_base_to_cfg_space(internal_id_bs, self.child_cfg_bases[self._id_ext])
                internal_ag_bs = self._add_base_to_cfg_space(internal_ag_bs, self.child_cfg_bases[self._ag_ext])
                internal_sg_bs = self._add_base_to_cfg_space(internal_sg_bs, self.child_cfg_bases[self._sg_ext])
                # add on the respective base
                # Now get the output part
                vec_out_addr_map = vec_out['address']
                external_ag_bs = self._ag_int.gen_bitstream(address_map=vec_out_addr_map)
                external_ag_bs = self._add_base_to_cfg_space(external_ag_bs, self.child_cfg_bases[self._ag_int])

                all_bs = [internal_id_bs, internal_ag_bs, internal_sg_bs, external_ag_bs]

            elif self.get_direction() == Direction.OUT:
                vec_in_addr_map = vec_in['address']
                vec_in_sched_map = vec_in['schedule']
                internal_id_bs = self._id_int.gen_bitstream(dimensionality=vec_in['dimensionality'],
                                                            extents=vec_in['extents'])
                internal_ag_bs = self._ag_int.gen_bitstream(address_map=vec_in_addr_map)
                internal_sg_bs = self._sg_int.gen_bitstream(schedule_map=vec_in_sched_map)

                internal_id_bs = self._add_base_to_cfg_space(internal_id_bs, self.child_cfg_bases[self._id_int])
                internal_ag_bs = self._add_base_to_cfg_space(internal_ag_bs, self.child_cfg_bases[self._ag_int])
                internal_sg_bs = self._add_base_to_cfg_space(internal_sg_bs, self.child_cfg_bases[self._sg_int])

                # Now get the output part
                vec_out_addr_map = vec_out['address']
                external_ag_bs = self._ag_ext.gen_bitstream(address_map=vec_out_addr_map)
                external_ag_bs = self._add_base_to_cfg_space(external_ag_bs, self.child_cfg_bases[self._ag_ext])
                all_bs = [internal_id_bs, internal_ag_bs, internal_sg_bs, external_ag_bs]

            else:
                raise NotImplementedError

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
