from lake.spec.component import Component
from lake.utils.spec_enum import *
from kratos import always_ff, always_comb, posedge, clog2
from lake.top.memory_interface import MemoryInterface
from lake.utils.util import register
import kratos as kts
import math
from lake.spec.memory_port import PhysicalMemoryPort
from kratos.stmts import *
from lake.spec.memory_port import MemoryPort


class Storage(Component):

    def __init__(self, capacity=1024, tech_map=None):
        super().__init__()
        self._capacity = capacity
        self.memport_sets = {}
        self.tech_map = tech_map
        self.memory_ports = None

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()

    def get_capacity(self):
        return self._capacity

    def get_memport_intfs(self):
        return self.memport_sets

    def set_tech_map(self, tech_map):
        self.tech_map = tech_map


class SingleBankStorage(Storage):

    def __init__(self, capacity=1024, tech_map=None):
        super().__init__(capacity=capacity, tech_map=tech_map)

    def _build_array(self):
        self._hw_array = self.var("data_array", self._capacity * 8)

    def _map_array_to_intf(self, tmp_intf, mp_width, num_addrs, rw=0):
        for i_ in range(num_addrs):
            for j_ in range(mp_width):
                if rw == 1:
                    self.wire(self._hw_array[i_ * mp_width + j_], tmp_intf[i_][j_])
                else:
                    self.wire(tmp_intf[i_][j_], self._hw_array[i_ * mp_width + j_])

    def create_interface(self):

        # For those with actual tech maps
        # self.use_physical = self.parameter("use_physical", 1)
        # Loop through the memory ports and create the proper signals - only need a few signals
        # for pnum, port in enumerate(self.mem_ports):
        #     local_memport_set = {}

        #     port_type = port.get_port_type()
        #     port_intf = port.get_port_interface()
        #     if port_type == MemoryPortType.READ:
        #         # Read port has data out, address, and ren
        #         port_intf['data_out'] = self.output(f"data_out_p{pnum}", self.mem_width, packed=True)
        #         port_intf['read_addr'] = self.input(f"read_addr_p{pnum}", kts.clog2(self.mem_depth), packed=True)
        #         port_intf['read_enable'] = self.input(f"read_enable_p{pnum}", 1)
        #         local_memport_set['type'] = MemoryPortType.R
        #         local_memport_set['addr'] =  port_intf['read_addr']
        #         local_memport_set['read_data'] = port_intf['data_out']
        #         local_memport_set['read_en'] =  port_intf['read_enable']
        #     elif port_type == MemoryPortType.WRITE:
        #         port_intf['data_in'] = self.input(f"data_in_p{pnum}", self.mem_width, packed=True)
        #         port_intf['write_addr'] = self.input(f"write_addr_p{pnum}", kts.clog2(self.mem_depth), packed=True)
        #         port_intf['write_enable'] = self.input(f"write_enable_p{pnum}", 1)
        #     elif port_type == MemoryPortType.READWRITE:
        #         port_intf['data_out'] = self.output(f"data_out_p{pnum}", self.mem_width, packed=True)
        #         port_intf['read_addr'] = self.input(f"read_addr_p{pnum}", kts.clog2(self.mem_depth), packed=True)
        #         port_intf['read_enable'] = self.input(f"read_enable_p{pnum}", 1)
        #         port_intf['data_in'] = self.input(f"data_in_p{pnum}", self.mem_width, packed=True)
        #         port_intf['write_addr'] = self.input(f"write_addr_p{pnum}", kts.clog2(self.mem_depth), packed=True)
        #         port_intf['write_enable'] = self.input(f"write_enable_p{pnum}", 1)

        #     self.memport_sets[pnum] = local_memport_set

        for pnum, mem_port in enumerate(self.memory_ports):
            local_memport_set = {}
            # Get the memoryport information
            mp_width = mem_port.get_width()
            mp_type = mem_port.get_type()
            num_addrs = self._capacity // (mp_width // 8)
            mem_port.set_num_addrs(num_addrs)
            addr_width = clog2(num_addrs)
            # mp_width = const(mp_width, addr_width)
            # tmp_intf = None
            if mp_type == MemoryPortType.R:
                # For read, we just need addr, ren, read data
                # tmp_intf = self.var(f"mem_intf_r_{pnum}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                # self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=0)
                print(addr_width)
                addr = self.input(f"memory_port_{pnum}_read_addr", addr_width)
                data = self.output(f"memory_port_{pnum}_read_data", mp_width)
                en = self.input(f"memory_port_{pnum}_read_en", 1)
                local_memport_set['type'] = MemoryPortType.R
                local_memport_set['addr'] = addr
                local_memport_set['read_data'] = data
                local_memport_set['read_en'] = en

                # @always_ff((posedge, "clk"))
                # def materialize_r(self):
                #     if en:
                #         data = tmp_intf[addr]
                # self.add_code(materialize_r)

            elif mp_type == MemoryPortType.W:
                # For write, we just need addr, wen, write data
                # tmp_intf = self.var(f"mem_intf_w_{pnum}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                # self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=1)
                addr = self.input(f"memory_port_{pnum}_write_addr", addr_width)
                data = self.input(f"memory_port_{pnum}_write_data", mp_width)
                en = self.input(f"memory_port_{pnum}_write_en", 1)
                local_memport_set['type'] = MemoryPortType.W
                local_memport_set['addr'] = addr
                local_memport_set['write_data'] = data
                local_memport_set['write_en'] = en

                # @always_ff((posedge, "clk"))
                # def materialize_w():
                #     if en:
                #         # self._hw_array[0, 0] = data[0, 0]
                #         tmp_intf[addr] = data

                # self.add_code(materialize_w)
            elif mp_type == MemoryPortType.RW:
                tmp_intf_r = self.var(f"mem_intf_rw_{pnum}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                # self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=0)
                # tmp_intf_w = self.var(f"mem_intf_w_{pnum}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                # self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=1)
                addr = self.input(f"memory_port_{pnum}_addr", addr_width)
                wdata = self.input(f"memory_port_{pnum}_write_data", mp_width)
                rdata = self.output(f"memory_port_{pnum}_read_data", mp_width)
                wen = self.input(f"memory_port_{pnum}_write_en", 1)
                ren = self.input(f"memory_port_{pnum}_read_en", 1)
                local_memport_set['type'] = MemoryPortType.RW
                local_memport_set['addr'] = addr
                local_memport_set['read_data'] = rdata
                local_memport_set['write_data'] = wdata
                local_memport_set['read_en'] = ren
                local_memport_set['write_en'] = wen

                # @always_ff((posedge, "clk"))
                # def materialize_rw():
                #     if wen:
                #         tmp_intf_w[addr] = wdata
                #     elif ren:
                #         rdata = tmp_intf_r[addr]
                # self.add_code(materialize_rw)
            # For read/write, we just need addr, wen, ren, write data, read data
                pass
            else:
                raise NotImplementedError

            self.memport_sets[pnum] = local_memport_set

            # mp_num += 1

        # Create alt sigs that need wires and not hardcoded values
        # if not self.sim_macro_n:
        # if True:
        #     assert self.tech_map is not None
        #     for p_ in self.tech_map['ports']:
        #         alt_sigs_ = p_['alt_sigs']
        #         for alt_sig__, v_width_tuple in alt_sigs_.items():
        #             sig_name = alt_sig__
        #             value, width = v_width_tuple
        #             if type(value) is not int:
        #                 # Then we create an interface port here
        #                 # if it is an alt signal with a non-constant value
        #                 port_intf[alt_sig__] = self.input(f"{alt_sig__}", width)

    def gen_hardware(self, pos_reset=False, memory_ports=None):

        assert memory_ports is not None
        if type(memory_ports) is not list:
            self.memory_ports = list(memory_ports)
        else:
            self.memory_ports = memory_ports

        # For building this with tech map, guarantee that memory ports are all the same size
        if self.tech_map is not None:
            all_ports_same_width = True
            port_width = self.memory_ports[0].get_width()
            for mp in memory_ports:
                if mp.get_width() != port_width:
                    all_ports_same_width = False
            assert all_ports_same_width

        self.create_interface()

        self.realize_hw()

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()
        return self.memport_sets

    def gen_bitstream(self):
        return super().gen_bitstream()

    def set_tech_map(self, tech_map):
        assert tech_map is not None, f"Need to provide valid tech map"
        self.tech_map = tech_map
        self.tech_map_provided = True

    def realize_hw(self):
        if self.tech_map is None:
            self.create_simulatable_memory()
        else:
            assert self.tech_map is not None, f"Need to provide tech map for macro realization"
            self.create_physical_memory()

        # Probably need to resolve address conflicts?
        self.resolve_addr_conflicts()

    def resolve_addr_conflicts(self):
        pass

    def get_ports(self):
        return self.memory_ports

    def get_num_ports(self):
        return len(self.mem_ports)

    def create_simulatable_memory(self):
        '''
        Based on the ports within the memory
        '''
        self._build_array()
        # Give each memory port an interface into the memory

        for pnum, mem_port in enumerate(self.memory_ports):

            mem_port: MemoryPort

            local_memport_set = self.memport_sets[pnum]
            # Get the memoryport information
            mp_width = mem_port.get_width()
            mp_type = mem_port.get_type()
            num_addrs = self._capacity // (mp_width // 8)
            addr_width = clog2(num_addrs)
            # mp_width = const(mp_width, addr_width)
            # tmp_intf = None
            if mp_type == MemoryPortType.R:
                # For read, we just need addr, ren, read data
                tmp_intf = self.var(f"mem_intf_r_{pnum}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=0)
                # addr = self.input(f"memory_port_{mp_num}_read_addr", addr_width)
                # data = self.output(f"memory_port_{mp_num}_read_data", mp_width)
                # en = self.input(f"memory_port_{mp_num}_read_en", 1)
                # local_memport_set['type'] = MemoryPortType.R
                # local_memport_set['addr'] = addr
                # local_memport_set['read_data'] = data
                # local_memport_set['read_en'] = en
                addr = local_memport_set['addr']
                data = local_memport_set['read_data']
                en = local_memport_set['read_en']

                memport_delay = mem_port.get_port_delay()

                if memport_delay == 0:

                    @always_comb
                    def materialize_r(self):
                        data = 0
                        if en:
                            data = tmp_intf[addr]
                    self.add_code(materialize_r)

                else:

                    @always_ff((posedge, "clk"))
                    def materialize_r(self):
                        if en:
                            data = tmp_intf[addr]
                    self.add_code(materialize_r)

            elif mp_type == MemoryPortType.W:
                # For write, we just need addr, wen, write data
                tmp_intf = self.var(f"mem_intf_w_{pnum}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=1)

                addr = local_memport_set['addr']
                data = local_memport_set['write_data']
                en = local_memport_set['write_en']

                @always_ff((posedge, "clk"))
                def materialize_w():
                    if en:
                        # self._hw_array[0, 0] = data[0, 0]
                        tmp_intf[addr] = data

                self.add_code(materialize_w)
            elif mp_type == MemoryPortType.RW:
                tmp_intf_r = self.var(f"mem_intf_r_{pnum}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                self._map_array_to_intf(tmp_intf=tmp_intf_r, mp_width=mp_width, num_addrs=num_addrs, rw=0)
                tmp_intf_w = self.var(f"mem_intf_w_{pnum}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                self._map_array_to_intf(tmp_intf=tmp_intf_w, mp_width=mp_width, num_addrs=num_addrs, rw=1)
                # addr = self.input(f"memory_port_{mp_num}_read_addr", addr_width)
                # wdata = self.input(f"memory_port_{mp_num}_write_data", mp_width)
                addr = local_memport_set['addr']
                wdata = local_memport_set['write_data']
                rdata = local_memport_set['read_data']
                wen = local_memport_set['write_en']
                ren = local_memport_set['read_en']

                @always_ff((posedge, "clk"))
                def materialize_rw():
                    if wen:
                        tmp_intf_w[addr] = wdata
                    elif ren:
                        rdata = tmp_intf_r[addr]
                self.add_code(materialize_rw)
            # For read/write, we just need addr, wen, ren, write data, read data
                pass
            else:
                raise NotImplementedError

        return

        # First create the data array...
        self._clk = self.clock("clk")
        if self.reset_in_sim:
            self._rst_n = self.reset("rst_n")
        self._data_array = self.var("data_array", width=self.mem_width, size=self.mem_depth)
        # Now add in the logic for the ports.
        for port in self.get_ports():
            port_type = port.get_port_type()
            if port_type == MemoryPortType.READ:
                self.realize_read_port(port)
            elif port_type == MemoryPortType.READWRITE:
                self.realize_readwrite_port(port)
            elif port_type == MemoryPortType.WRITE:
                self.realize_write_port(port)

    def realize_read_port(self, port: MemoryPort):
        pintf = port.get_port_interface()
        new_read = None
        # Standalone read ports may be comb (register file),
        # or they may be sequential if requiring an active read...
        # seq_comb_n = port.get_active_read() or (port.get_port_delay() > 1)
        seq_comb_n = port.get_port_delay() >= 1
        if seq_comb_n:
            # Gen sequential read...
            sens_lst = [(kts.posedge, self._clk)]
            new_read = self.sequential(*sens_lst)
            new_rd_if = new_read.if_(pintf['read_enable'] == 1)
            new_rd_if.then_(pintf['data_out'].assign(self._data_array[pintf['read_addr']]))
        else:
            # Gen combinational read...
            new_read = self.combinational()
            new_read.add_stmt(pintf['data_out'].assign(self._data_array[pintf['read_addr']]))

    def realize_readwrite_port(self, port: MemoryPort):
        # In reality, a read/write port has to have a read delay,
        # otherwise it would be a separate port - so we only handle this case...
        pintf = port.get_port_interface()
        sens_lst = [(kts.posedge, self._clk)]
        new_write = self.sequential(*sens_lst)
        new_wr_if = new_write.if_(pintf['write_enable'] == 1)
        new_wr_if.then_(self._data_array[pintf['write_addr']].assign(pintf['data_in']))
        # If delay is 1 or more, do active read
        if port.get_port_delay() > 0:
            assert port.get_active_read() is True
            read_if = IfStmt(pintf['read_enable'])
            read_if.then_(pintf['data_out'].assign(self._data_array[pintf['read_addr']]))
            new_wr_if.else_(read_if)
        else:
            self.add_stmt(pintf['data_out'].assign(self._data_array[pintf['read_addr']]))

    def realize_write_port(self, port: MemoryPort):
        pintf = port.get_port_interface()
        sens_lst = [(kts.posedge, self._clk)]
        new_write = self.sequential(*sens_lst)
        new_wr_if = new_write.if_(pintf['write_enable'] == 1)
        new_wr_if.then_(self._data_array[pintf['write_addr']].assign(pintf['data_in']))

    def realize_read_port_phys(self, logical: MemoryPort, physical: PhysicalMemoryPort, port_idx: int):
        # l_pint = logical.get_port_interface()
        l_pint = self.memport_sets[port_idx]
        p_pint = physical.get_port_interface()
        # Just wire things through for this...
        self.wire(self._clk, p_pint['clk'])
        self.wire(l_pint['read_data'], p_pint['read_data'])
        self.wire(l_pint['addr'], p_pint['addr'])
        if physical.get_active_low():
            self.wire(~l_pint['read_en'], p_pint['cen'])
        else:
            self.wire(l_pint['read_en'], p_pint['cen'])

    def realize_readwrite_port_phys(self, logical: MemoryPort, physical: PhysicalMemoryPort, port_idx: int, write_prio=True):
        # In reality, a read/write port has to have a read delay,
        # otherwise it would be a separate port - so we only handle this case...
        # l_pint = logical.get_port_interface()
        l_pint = self.memport_sets[port_idx]
        p_pint = physical.get_port_interface()
        # Just wire things through for this...
        print("Printing logical interface")
        print(l_pint)
        print(p_pint)
        self.wire(self._clk, p_pint['clk'])
        self.wire(l_pint['read_data'], p_pint['read_data'])
        self.wire(l_pint['write_data'], p_pint['write_data'])

        # We are actually handling the address from the logical port in the memory port
        # self.wire(kts.ternary(l_pint['write_en'],
        #                       l_pint['write_addr'],
        #                       l_pint['read_addr']), p_pint['addr'])
        self.wire(l_pint['addr'], p_pint['addr'])

        cen_on = 'cen' in p_pint
        # Handle wen and cen
        if physical.get_active_low():
            self.wire(~l_pint['write_en'], p_pint['write_en'])
            if cen_on:
                self.wire(~(l_pint['write_en'] | l_pint['read_en']), p_pint['cen'])
            else:
                if write_prio:
                    self.wire(~(l_pint['read_en'] & ~l_pint['write_en']), p_pint['read_en'])
                else:
                    self.wire(~l_pint['read_en'], p_pint['read_en'])
        else:
            self.wire(l_pint['write_en'], p_pint['write_en'])
            if cen_on:
                self.wire((l_pint['write_en'] | l_pint['read_en']), p_pint['cen'])
            else:
                if write_prio:
                    self.wire(l_pint['read_en'] & ~l_pint['write_en'], p_pint['read_en'])
                else:
                    self.wire(l_pint['read_en'], p_pint['read_en'])

    def realize_write_port_phys(self, logical: MemoryPort, physical: PhysicalMemoryPort, port_idx: int):
        # l_pint = logical.get_port_interface()
        l_pint = self.memport_sets[port_idx]
        p_pint = physical.get_port_interface()
        # Just wire things through for this...
        self.wire(self._clk, p_pint['clk'])
        self.wire(l_pint['write_data'], p_pint['write_data'])
        self.wire(l_pint['addr'], p_pint['addr'])
        if physical.get_active_low():
            self.wire(~l_pint['write_en'], p_pint['write_en'])
            self.wire(~l_pint['write_en'], p_pint['cen'])
        else:
            self.wire(l_pint['write_en'], p_pint['write_en'])
            self.wire(l_pint['write_en'], p_pint['cen'])

    def create_physical_memory(self):
        '''
        Creating the physical memory is really just passing the ports through
        to the physical macro provided from some memory compiler + adding in logic
        for active low signals + chip enable
        '''
        # The tech map will provide almost identical information
        assert self.tech_map is not None, f"Need to provide a tech map before creating physical memory"

        # Tech map basically provides a name of the module and a list of port maps

        # First create wrapper clock
        # self._clk = self.clock("clk")

        # TODO - multiple clocks available sometimes

        # The port list
        port_maps = self.tech_map['ports']
        self.physical_ports = []

        print(self.get_ports)

        # Create physical ports to map the logical ports into
        for (idx, port) in enumerate(self.get_ports()):
            new_phys_port = PhysicalMemoryPort(port.get_type(), delay=1, active_read=True,
                                               active=self.tech_map['active'], port_map=port_maps[idx])
            self.physical_ports.append(new_phys_port)

        # Create the stub generator

        # Only get here if each memory port has the same interface width
        self.mem_params = {}
        self.mem_params['mem_width'] = self.memory_ports[0].get_width()
        self.mem_params['mem_depth'] = self.memory_ports[0].get_num_addrs()

        child_stub = PhysicalMemoryStub(name=self.tech_map['name'],
                                        mem_params=self.mem_params,
                                        ports=self.physical_ports,
                                        tech_map=self.tech_map)
        self.add_child("mem_stub", child_stub)

        # Now wire the logical ports to the physical ports
        log_ports = self.get_ports()
        phy_ports = self.physical_ports

        for port_idx in range(len(log_ports)):
            log_port = log_ports[port_idx]
            phy_port = phy_ports[port_idx]
            port_type = log_port.get_type()
            assert port_type == phy_port.get_type()
            if port_type == MemoryPortType.READ:
                self.realize_read_port_phys(logical=log_port, physical=phy_port, port_idx=port_idx)
            elif port_type == MemoryPortType.READWRITE:
                self.realize_readwrite_port_phys(logical=log_port, physical=phy_port, port_idx=port_idx)
            elif port_type == MemoryPortType.WRITE:
                self.realize_write_port_phys(logical=log_port, physical=phy_port, port_idx=port_idx)

            # Now set the arbitrary test signals
            alt_sigs = phy_port.get_alt_signals()
            if alt_sigs is not None:
                for (alt_sig, (value, width)) in alt_sigs.items():
                    phy_intf = phy_port.get_port_interface()
                    if type(value) is not int:
                        # If it is not a value, then it is a signal that must be handed down through
                        # the MemoryInterface parent generator
                        self.wire(phy_intf[alt_sig], log_port.get_port_interface()[alt_sig])
                    else:
                        self.wire(phy_intf[alt_sig], kts.const(value=value, width=width))


class PhysicalMemoryStub(kts.Generator):
    def __init__(self, name: str, debug=True, mem_params: dict = None, ports: list = None, tech_map: dict = None):
        super().__init__(name, debug=True)
        if mem_params is None:
            self.mem_params = {}
        else:
            self.mem_params = mem_params
        if ports is None:
            self.ports = []
        else:
            self.ports = ports
        assert tech_map is not None, f"tech_map is None"
        self.tech_map = tech_map
        # The name will be the name of the stub
        # Set external to true so that the internals aren't generated when instantiating
        self.external = True
        # Literally the IO for the stub
        self.mem_params = mem_params
        self.ports = ports
        self.mem_width = MemoryInterface.mem_width_dflt if 'mem_width' not in mem_params else mem_params['mem_width']
        self.mem_depth = MemoryInterface.mem_depth_dflt if 'mem_depth' not in mem_params else mem_params['mem_depth']
        self.composed = False

        # First create the top level interface to the physical sram
        self.create_interface()
        # Then run a pass to instantiate more of the tech_map
        self.compose_mems()

    def create_interface(self):

        # Create the port interfaces - clocks belong here now since there are actually clocks
        for port in self.get_ports():
            port_intf = port.get_port_interface()
            port_type = port.get_type()
            # port_type = port.get_port_type()
            # The slight deviation here
            if port_type == MemoryPortType.READ:
                # Read port has data out, address, and ren
                port_intf['read_data'] = self.output(port_intf['read_data'], self.mem_width, packed=True)
                port_intf['addr'] = self.input(port_intf['addr'], kts.clog2(self.mem_depth), packed=True)
                port_intf['cen'] = self.input(port_intf['cen'], 1)
                port_intf['clk'] = self.clock(port_intf['clk'])
            elif port_type == MemoryPortType.WRITE:
                port_intf['write_data'] = self.input(port_intf['write_data'], self.mem_width, packed=True)
                port_intf['addr'] = self.input(port_intf['addr'], kts.clog2(self.mem_depth), packed=True)
                port_intf['write_en'] = self.input(port_intf['write_en'], 1)
                port_intf['cen'] = self.input(port_intf['cen'], 1)
                port_intf['clk'] = self.clock(port_intf['clk'])
            elif port_type == MemoryPortType.READWRITE:
                port_intf['write_data'] = self.input(port_intf['write_data'], self.mem_width, packed=True)
                port_intf['read_data'] = self.output(port_intf['read_data'], self.mem_width, packed=True)
                port_intf['write_en'] = self.input(port_intf['write_en'], 1)
                addr_proxy = self.input(port_intf['addr'], kts.clog2(self.mem_depth), packed=True)
                port_intf['addr'] = addr_proxy
                if 'read_en' in port_intf:
                    port_intf['read_en'] = self.input(port_intf['read_en'], 1)
                else:
                    port_intf['cen'] = self.input(port_intf['cen'], 1)
                port_intf['clk'] = self.clock(port_intf['clk'])
            # For now, assume the alt sigs are all inputs
            for (alt_sig, (value, width)) in port.get_alt_signals().items():
                port_intf[alt_sig] = self.input(str(alt_sig), width)

    def compose_mems(self):
        '''
            compose_mems will find out if the tech map mismatches the dimensions of the memory and
            create a composed array of them to match the final interface
        '''
        tech_depth = self.tech_map['depth']
        tech_width = self.tech_map['width']
        num_deep = 1
        if tech_depth == self.mem_depth and tech_width == self.mem_width:
            # If the dimensions match there's nothing to do....
            return
        # Mark this as a composed memory so we know how to deal with stubbing later.
        self.composed = True
        self.external = False
        if tech_depth < self.mem_depth:
            num_deep = math.ceil(self.mem_depth / tech_depth)
        num_wide = 1
        if tech_width < self.mem_width:
            num_wide = math.ceil(self.mem_width / tech_width)
        self.name = self.name + f"_{num_wide}_wide_{num_deep}_deep"
        child_mem_params = {
            'mem_width': tech_width,
            'mem_depth': tech_depth
        }

        # Address range for decoding
        addr_bottom = kts.clog2(tech_depth)
        addr_top = addr_bottom + kts.clog2(num_deep) - 1
        self.composed_children = {}
        for x in range(num_wide):
            for y in range(num_deep):
                port_copies = []
                for port in self.ports:
                    # We have a physicalmemoryport here - copy it to the new stub
                    pt = port.get_type()
                    pd = port.get_port_delay()
                    pal = port.get_active_low()
                    pm = port.get_port_map()
                    port_copies.append(PhysicalMemoryPort(pt, pd, self.tech_map['active'], pal, pm))
                self.composed_children[(x, y)] = PhysicalMemoryStub(name=self.tech_map['name'],
                                                                    mem_params=child_mem_params,
                                                                    ports=port_copies,
                                                                    tech_map=self.tech_map)
                self.add_child(f"{self.tech_map['name']}_{x}_{y}",
                               self.composed_children[(x, y)])
                # Instantiated child, now need to hook up ports
        for i, port in enumerate(self.ports):
            port_intf = port.get_port_interface()
            port_type = port.get_type()
            port_delay = port.get_port_delay()
            # The slight deviation here
            if port_type == MemoryPortType.R:

                concat_data_outs = []

                for y in range(num_deep):
                    # Get the width of children gens
                    child_ports_wide = [self.composed_children[(x, y)] for x in range(num_wide)]
                    # concat their ports and handle broadcasting data, addr
                    child_ports_wide_intf = [child.get_ports()[i].get_port_interface() for child in child_ports_wide]
                    concat_data_out = kts.concat(*[cintf['data_out'] for cintf in child_ports_wide_intf])
                    concat_data_outs.append(concat_data_out)
                    # self.wire(port_intf['data_out'], concat_data_out)
                    # Deal with data out (in the delay version)
                    # concat_addr_in = kts.concat(*[cintf['read_addr'] for cintf in child_ports_wide_intf])
                    concat_addr_in = kts.concat(*[cintf['addr'] for cintf in child_ports_wide_intf])
                    # self.wire(port_intf['read_addr'], concat_addr_in)
                    self.wire(port_intf['addr'], concat_addr_in)
                    for ix in range(num_wide):
                        self.wire(port_intf['clk'], child_ports_wide_intf[ix]['clk'])
                        # Now decode cen...
                        if num_deep == 1:
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'])
                        else:
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'] & (port_intf['read_addr'][addr_top, addr_bottom] == y))

                # If only 1 deep, just wire it up and leave, otherwise create a combinational block
                if num_deep == 1:
                    self.add_stmt(port_intf['data_out'].assign(concat_data_outs[0]))
                else:

                    data_out_mux_sel = self.var(f"port_{i}_data_out_mux_sel", addr_top - addr_bottom + 1)
                    self.wire(data_out_mux_sel, port_intf['addr'][addr_top, addr_bottom])
                    # self.wire(data_out_mux_sel, port_intf['read_addr'][addr_top, addr_bottom])

                    if port_delay == 1:
                        final_mux_sel = register(self, data_out_mux_sel, enable=port_intf['cen'], name=f'port_{i}_registered_mux_sel_data_out')
                    else:
                        final_mux_sel = data_out_mux_sel

                    prev_stmt = None
                    data_out_comb = self.combinational()
                    data_out_comb.add_stmt(port_intf['data_out'].assign(0))
                    on_first = True
                    for y in range(num_deep):
                        if on_first is True:
                            first_if = data_out_comb.if_(final_mux_sel == kts.const(y, final_mux_sel.width))
                            first_if.then_(port_intf['data_out'].assign(concat_data_outs[y]))
                            prev_stmt = first_if
                            on_first = False
                        else:
                            chain_if = IfStmt(final_mux_sel == kts.const(y, final_mux_sel.width))
                            chain_if.then_(port_intf['data_out'].assign(concat_data_outs[y]))
                            prev_stmt.else_(chain_if)
                            prev_stmt = chain_if

            elif port_type == MemoryPortType.W:
                # Decode the cen/wen, broadcast data, addr to the mems
                for y in range(num_deep):
                    # Get the width of children gens
                    child_ports_wide = [self.composed_children[(x, y)] for x in range(num_wide)]
                    # concat their ports and handle broadcasting data, addr
                    child_ports_wide_intf = [child.get_ports()[i].get_port_interface() for child in child_ports_wide]
                    concat_data_in = kts.concat(*[cintf['write_data'] for cintf in child_ports_wide_intf])
                    self.wire(port_intf['write_data'], concat_data_in)
                    concat_addr_in = kts.concat(*[cintf['addr'] for cintf in child_ports_wide_intf])
                    self.wire(port_intf['addr'], concat_addr_in)
                    for ix in range(num_wide):
                        self.wire(port_intf['clk'], child_ports_wide_intf[ix]['clk'])
                        # Now decode write enable and cen through here...
                        if num_deep == 1:
                            self.wire(child_ports_wide_intf[ix]['write_enable'], port_intf['write_enable'])
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'])
                        else:
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'] & (port_intf['write_addr'][addr_top, addr_bottom] == y))
                            self.wire(child_ports_wide_intf[ix]['write_enable'], port_intf['write_enable'] & (port_intf['write_addr'][addr_top, addr_bottom] == y))

            elif port_type == MemoryPortType.RW:

                concat_data_outs = []

                for y in range(num_deep):
                    # Get the width of children gens
                    child_ports_wide = [self.composed_children[(x, y)] for x in range(num_wide)]
                    # concat their ports and handle broadcasting data, addr
                    child_ports_wide_intf = [child.get_ports()[i].get_port_interface() for child in child_ports_wide]
                    concat_data_out = kts.concat(*[cintf['data_out'] for cintf in child_ports_wide_intf])
                    concat_data_outs.append(concat_data_out)

                    # self.wire(port_intf['data_out'], concat_data_out)
                    # Deal with data out (in the delay version)
                    concat_data_in = kts.concat(*[cintf['data_in'] for cintf in child_ports_wide_intf])
                    self.wire(port_intf['data_in'], concat_data_in)
                    # concat_addr_in = kts.concat(*[cintf['addr'] for cintf in child_ports_wide_intf])
                    # self.wire(port_intf['addr'], concat_addr_in)
                    for ix in range(num_wide):
                        self.wire(port_intf['clk'], child_ports_wide_intf[ix]['clk'])
                        self.wire(port_intf['addr'], child_ports_wide_intf[ix]['addr'])
                        # Now decode and cen through here...
                        if num_deep == 1:
                            self.wire(child_ports_wide_intf[ix]['write_enable'], port_intf['write_enable'])
                            if 'read_enable' in port_intf:
                                self.wire(child_ports_wide_intf[ix]['read_enable'], port_intf['read_enable'])
                            else:
                                self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'])
                        else:
                            if 'read_enable' in port_intf:
                                self.wire(child_ports_wide_intf[ix]['read_enable'], port_intf['read_enable'] & (port_intf['addr'][addr_top, addr_bottom] == y))
                            else:
                                self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'] & (port_intf['addr'][addr_top, addr_bottom] == y))
                            self.wire(child_ports_wide_intf[ix]['write_enable'], port_intf['write_enable'] & (port_intf['addr'][addr_top, addr_bottom] == y))

                # If only 1 deep, just wire it up and leave, otherwise create a combinational block
                if num_deep == 1:
                    self.add_stmt(port_intf['data_out'].assign(concat_data_outs[0]))
                else:

                    data_out_mux_sel = self.var(f"port_{i}_data_out_mux_sel", addr_top - addr_bottom + 1)
                    self.wire(data_out_mux_sel, port_intf['addr'][addr_top, addr_bottom])

                    if port_delay == 1:
                        # Only update the read out mux when it's a read
                        final_mux_sel = register(self, data_out_mux_sel, enable=port_intf['cen'] & ~port_intf['write_enable'], name=f'port_{i}_registered_mux_sel_data_out')
                    else:
                        final_mux_sel = data_out_mux_sel

                    # Build combinational mux - can't directly index since I created the object list
                    prev_stmt = None
                    data_out_comb = self.combinational()
                    data_out_comb.add_stmt(port_intf['data_out'].assign(0))
                    on_first = True
                    for y in range(num_deep):
                        if on_first is True:
                            first_if = data_out_comb.if_(final_mux_sel == kts.const(y, final_mux_sel.width))
                            first_if.then_(port_intf['data_out'].assign(concat_data_outs[y]))
                            prev_stmt = first_if
                            on_first = False
                        else:
                            chain_if = IfStmt(final_mux_sel == kts.const(y, final_mux_sel.width))
                            chain_if.then_(port_intf['data_out'].assign(concat_data_outs[y]))
                            prev_stmt.else_(chain_if)
                            prev_stmt = chain_if

            # For now, assume the alt sigs are all inputs
            for xx in range(num_wide):
                for yy in range(num_deep):
                    child_port_intf = self.composed_children[(xx, yy)].get_ports()[i].get_port_interface()
                    for (alt_sig, (value, width)) in port.get_alt_signals().items():
                        self.wire(child_port_intf[alt_sig], port_intf[alt_sig])

    def get_ports(self):
        return self.ports
