import networkx as nx
import matplotlib.pyplot as plt
from lake.spec.storage import Storage
from lake.spec.memory_port import MemoryPort
from lake.utils.spec_enum import *
from lake.spec.iteration_domain import IterationDomain
from lake.spec.address_generator import AddressGenerator
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.port import Port
from lake.modules.memory_interface_decoder import MemoryInterfaceDecoder
import kratos as kts
from kratos import clog2
from typing import Tuple
import os as os
from lake.utils.spec_enum import Direction
from lake.utils.util import connect_memoryport_storage, inline_multiplexer
from lake.modules.rv_comparison_network import RVComparisonNetwork
from lake.spec.component import Component


class Spec():
    """Harness for lake primitives
    """
    def __init__(self, name="lakespec") -> None:
        self._hw_graph = nx.Graph()
        self._final_gen = None
        self._name = name
        self._memport_map = {}
        self._num_nodes = 0
        self._index_to_node = {}
        self._node_to_index = {}
        # Each node by index will have its base here
        self._config_bases = []
        self.configuration = None
        self.config_int = None
        self.total_config_size = None
        self.mp_to_mid = None
        self.any_rv_sg = False
        self.rv_comparison_network = None
        self.num_ports = 0

    def register_(self, comp):
        self._hw_graph.add_node(comp)
        self._hw_graph[comp]['index'] = self._num_nodes
        self._index_to_node[self._num_nodes] = comp
        self._num_nodes += 1

    def get_num_ports(self):
        return self.num_ports

    def get_node_from_idx(self, idx):
        print(idx)
        print(self._index_to_node)
        assert idx in self._index_to_node
        return self._index_to_node[idx]

    def register(self, *comps):
        # self._hw_graph.add_nodes_from(comps)
        for comp in comps:
            # self.register_(comp=comp)
            if isinstance(comp, ScheduleGenerator) and comp.get_rv():
                self.any_rv_sg = True
            if isinstance(comp, Port):
                self.num_ports += 1

            self._hw_graph.add_node(comp)
            self._node_to_index[comp] = self._num_nodes
            self._index_to_node[self._num_nodes] = comp
            self._num_nodes += 1

    def connect(self, node1, node2):
        self._hw_graph.add_edge(node1, node2)
        # If this is connecting a memoryport and a port then we need to add it to the map
        if (isinstance(node1, MemoryPort) and isinstance(node2, Port)) or (isinstance(node1, Port) and isinstance(node2, MemoryPort)):
            if isinstance(node1, MemoryPort):
                node_ = node1
                node_other = node2
            else:
                node_ = node2
                node_other = node1
            if node_ not in self._memport_map:
                self._memport_map[node_] = []
            self._memport_map[node_].append(node_other)

    def visualize_graph(self) -> None:
        nx.draw(self._hw_graph, with_labels=False, font_weight='bold')
        plt.savefig('graph_mek.png', dpi=300)

    def get_nodes(self, node_type) -> list:
        ret_list = list()
        for node in self._hw_graph.nodes():
            # print(node)
            # print(node_type)
            isinst = isinstance(node, node_type)
            if isinst:
                # Then I should add to list
                ret_list.append(node)
        return ret_list

    def get_associated_controller(self, associated_type, node):
        for neighbor in self._hw_graph.neighbors(node):
            isinst = isinstance(neighbor, associated_type)
            if isinst:
                return neighbor
        return None

    def get_memory_ports(self, port):
        ret_list = list()
        for neighbor in self._hw_graph.neighbors(port):
            if isinstance(neighbor, MemoryPort):
                ret_list.append(neighbor)
        return ret_list

    def lift_config_regs(self):
        # Go through and grab the config space of every node in the design
        total_config_size = 0

        for node in self._hw_graph.nodes:
            print(node.get_name())
            # The config bases will contain a number for each node - should match?
            self._config_bases.append(total_config_size)
            total_config_size += node.get_config_size()

        self.total_config_size = total_config_size

        self.config_memory = self._final_gen.input(name=f"config_memory", width=total_config_size, packed=True)
        base = 0
        for node in self._hw_graph.nodes:
            cfgspc = node.get_config_space()
            for (node_config_size, signal_name_lcl) in cfgspc:
                self._final_gen.wire(self.config_memory[base + node_config_size - 1, base], signal_name_lcl)
                base += node_config_size
        # Concatenate them

    def generate_hardware(self) -> None:

        # self._final_gen = kts.Generator(name=self._name, debug=False)
        print(self._name)
        self._final_gen = Component(name=self._name)
        # self._config_memory_size = self._final_gen.parameter('CFG_SIZE', initial_value=1)

        # self._final_gen.clk = self._final_gen.clock("clk")

        # Before we go into the port loop, if any of the schedule generators is dynamic (vs. static), we need to know this and collect
        # information before genning the hardware
        # any_rv_sg = True
        num_writes = 1
        num_reads = 1

        # Just instantiate one comparison thing for now...
        if self.any_rv_sg:
            # In later stage of spec hw gen will add each read and write port to this...
            self.rv_comparison_network = RVComparisonNetwork(name='rv_comparison_network_this_spec')

        self.hw_attr = {}
        self.hw_attr['clk'] = self._final_gen.clock("clk")
        self.hw_attr['rst_n'] = self._final_gen.reset("rst_n")

        # First generate the storages based on the ports connected to them and their capacities
        storage_nodes = self.get_nodes(Storage)
        print(storage_nodes)

        for j_, storage_node in enumerate(storage_nodes):
            storage_node: Storage
            print('in storage')
            # get MemoryPorts
            memoryports = list(nx.neighbors(self._hw_graph, storage_node))
            storage_node.gen_hardware(pos_reset=False, memory_ports=memoryports)
            # memoryports = nx.neighbors(self._hw_graph, storage_node)
            # Now we have the storage generated, want to generate the memoryports hardware which will be simply
            # passthru of the port currently...
            for memoryport in memoryports:
                print('going through memports')
                print(memoryport.get_name())
            print('aftert storage')
            # Now generate a storage element based on all of these ports and add them to the final generator
            self._final_gen.add_child("storage", storage_node, clk=self.hw_attr['clk'])

            strg_intfs = storage_node.get_memport_intfs()

            # Build memory ports
            memoryports = nx.neighbors(self._hw_graph, storage_node)
            for i_, mp in enumerate(memoryports):
                mp: MemoryPort
                print('mek')
                print(mp.get_name())
                mp.gen_hardware(pos_reset=False, storage_node=storage_node)
                self._final_gen.add_child(f"memoryport_{i_}_storage_{j_}", mp)
                # self._connect_memoryport_storage(mptype=mp.get_type(), memport_intf=mp.get_storage_intf(), strg_intf=strg_intfs[i_])
                connect_memoryport_storage(self._final_gen, mptype=mp.get_type(), memport_intf=mp.get_storage_intf(), strg_intf=strg_intfs[i_])
                # Connected the memory ports to the storage

        # Now that we have generated the memory ports and storage, we can realize
        # the ports and supporting hardware

        port_nodes = self.get_nodes(Port)
        for i_, port in enumerate(port_nodes):
            port: Port

            port_direction = port.get_direction()

            port_id, port_ag, port_sg = self.get_port_controllers(port=port)
            # Assemble the ID,AG,SG (or at least their interfaces) on the port
            # port_id: IterationDomain = self.get_associated_controller(IterationDomain, port)
            # assert port_id is not None
            # port_ag: AddressGenerator = self.get_associated_controller(AddressGenerator, port)
            # assert port_ag is not None
            # port_sg: ScheduleGenerator = self.get_associated_controller(ScheduleGenerator, port)
            # assert port_sg is not None

            # Connect port's data to all of the memoryports
            id_dims = port_id.get_dimensionality()
            memports_ = self.get_memory_ports(port=port)
            # Port needs to know about the dimensionality in case of a vectorized port to
            # build the proper hardware within the port
            port_id.gen_hardware()
            port_ag.gen_hardware(memports_, port_id)

            if self.any_rv_sg:
                # We need to include some information about the other ports when building schedule generator
                if port_direction == Direction.IN:
                    port_sg.gen_hardware(id=port_id, num_comparisons=num_reads)
                elif port_direction == Direction.OUT:
                    port_sg.gen_hardware(id=port_id, num_comparisons=num_writes)
                else:
                    raise NotImplementedError()
            else:
                port_sg.gen_hardware(port_id)

            if self.any_rv_sg:
                # Just add the Port Direction and SG to the RVComparisonNetwork
                self.rv_comparison_network.add_reader_writer(direction=port_direction, sg=port_sg)

            port.gen_hardware(dimensionality=id_dims, external_id=port_id)

            self._final_gen.add_child(f"port_inst_{i_}", port,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'])
            # Connect the ag/sg/id together
            self._final_gen.add_child(f"port_id_{i_}", port_id,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'])
            self._final_gen.add_child(f"port_ag_{i_}", port_ag,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'])
            self._final_gen.add_child(f"port_sg_{i_}", port_sg,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'])

            self._final_gen.wire(port_id.ports.mux_sel, port_ag.ports.mux_sel)
            self._final_gen.wire(port_id.ports.iterators, port_ag.ports.iterators)

            self._final_gen.wire(port_id.ports.mux_sel, port_sg.ports.mux_sel)
            self._final_gen.wire(port_id.ports.iterators, port_sg.ports.iterators)

            quali_step = port_sg.ports.step
            # Here we need to qualify the port sg's step before sending it into the memory port, ID, SG
            if self.any_rv_sg:
                # If it is an IN Port, we need to qualify the step with the incoming data being valid
                # as well as the grant for the MemoryPort's arbitration
                # In this case, the step is the req line to the arbiter and the ready line to the data
                if port_direction == Direction.IN:
                    # quali_step = self.qualify_step(port_sg, Direction.IN)
                    quali_step = port_sg.ports.step
                # If it is an OUT Port, we need to qualify the step with the grant line from the arbitration and the
                # downstream ready (from the Port I guess)
                elif port_direction == Direction.OUT:
                    quali_step = port_sg.ports.step
                    # quali_step = self.qualify_step(port_sg, Direction.OUT)
                else:
                    raise NotImplementedError(f"Only support {Direction.IN} and {Direction.OUT}")

                # Now that we have the qualified steps to plug in everywhere, we also need to handle comparison hardware

            # This is in the case of a ready/valid system
            # step signal
            self._final_gen.wire(quali_step, port_ag.ports.step)
            self._final_gen.wire(port_sg.ports.step, port_id.ports.step)

            # If the port is wide fetch, we can wire the SG's step and IDs signals to the port
            if port.get_fw() > 1:
                ext_intf = port.get_internal_ag_intf()
                self._final_gen.wire(ext_intf['step'], quali_step)
                self._final_gen.wire(ext_intf['mux_sel'], port_id.ports.mux_sel)
                self._final_gen.wire(ext_intf['iterators'], port_id.ports.iterators)

            # Gen the hardware for each, assemble the signals
            assembled_port = {}
            assembled_port['dir'] = port.get_direction()
            assembled_port['data'] = port.get_mp_intf()['data']
            assembled_port['addr'] = port_ag.get_address()
            assembled_port['en'] = quali_step
            # assembled_port['en'] = port_sg.get_step()

            # send signals through memintf decoder (one port to many memoryports, doing decoding based on address)
            memintf_dec = MemoryInterfaceDecoder(name=f"memintfdec_{i_}", port_type=port.get_direction(),
                                                 port_intf=assembled_port, memports=memports_)
            memintf_dec.gen_hardware()

            self._final_gen.add_child(f"memintfdec_inst_{i_}", memintf_dec)
            # After this, need to connect the decoded ports to the actual memports
            mintf_ints = memintf_dec.get_mp_intf()
            for j_, mp in enumerate(memports_):
                self.register_mid_w_mp(mid=mintf_ints[j_], mp=mp)
                # self._connect_memintfdec_mp(mintf_ints[j_], mp)

            memintf_dec_p_intf = memintf_dec.get_p_intf()
            self._final_gen.wire(assembled_port['data'], memintf_dec_p_intf['data'])
            self._final_gen.wire(assembled_port['addr'], memintf_dec_p_intf['addr'])
            self._final_gen.wire(assembled_port['en'], memintf_dec_p_intf['en'])

            # Now add the port interfaces to the module
            ub_intf = port.get_ub_intf()
            if port.get_direction() == Direction.IN:
                p_temp = self._final_gen.input(f"port_{i_}", width=ub_intf['data'].width)
            elif port.get_direction() == Direction.OUT:
                p_temp = self._final_gen.output(f"port_{i_}", width=ub_intf['data'].width)
            self._final_gen.wire(p_temp, ub_intf['data'])

        # Now can build all the muxing in between the memintf and the mps
        self.build_mp_p()

        # Can now build the comparison network if it's there...
        if self.any_rv_sg:
            self.rv_comparison_network.gen_hardware()
            self._final_gen.add_child('rv_comp_network_top_spec', self.rv_comparison_network)
            rv_comp_conns = self.rv_comparison_network.get_connections()
            for conn_tuple in rv_comp_conns:
                p1, p2 = conn_tuple
                self._final_gen.wire(p1, p2)

        # self.lift_config_regs()
        print("building spec cfg memory input")
        self._final_gen._assemble_cfg_memory_input()
        self.add_flush()

    def get_verilog(self, output_dir):
        # kts.verilog(self._final_gen, filename=f"{self._name}.sv",
        fn_ = f"{self._name}.sv"
        full_path = os.path.join(output_dir, fn_)
        kts.verilog(self._final_gen, filename=full_path,
                    optimize_if=False)

    def add_flush(self):
        self._final_gen.add_attribute("sync-reset=flush")
        kts.passes.auto_insert_sync_reset(self._final_gen.internal_generator)
        flush_port = self._final_gen.internal_generator.get_port("flush")
        # flush_port.add_attribute(ControlSignalAttr(True))

    def extract_compiler_information(self) -> None:
        pass

    def get_port_controllers(self, port) -> Tuple[IterationDomain, AddressGenerator, ScheduleGenerator]:
        # Assemble the ID,AG,SG (or at least their interfaces) on the port
        port_id: IterationDomain = self.get_associated_controller(IterationDomain, port)
        assert port_id is not None
        port_ag: AddressGenerator = self.get_associated_controller(AddressGenerator, port)
        assert port_ag is not None
        port_sg: ScheduleGenerator = self.get_associated_controller(ScheduleGenerator, port)
        assert port_sg is not None

        return (port_id, port_ag, port_sg)

    def register_mid_w_mp(self, mid, mp):
        if self.mp_to_mid is None:
            self.mp_to_mid = {}

        if mp not in self.mp_to_mid:
            self.mp_to_mid[mp] = [mid]
        else:
            self.mp_to_mid[mp].append(mid)

    def build_mp_p(self, static=True):
        ''' We have to now build the arbitration and muxes between the Ports and
            their shared MemoryPorts - if `static` is `True`, arbitration will just be simple priority
            encoding as that won't matter since no two controllers will access the same
            resource on the same cycle
        '''
        if static:
            for mp, mid_intf_lst in self.mp_to_mid.items():
                # We have a memory port and a set of connections to go to it - build a mux for
                # addr, data, en
                num_mids = len(mid_intf_lst)
                # Use a one-hot select line mux
                sels = None
                ens = None
                addrs = None
                data = None
                mp_type = mp.get_type()
                mp_port_intf = mp.get_port_intf()
                # all_mux_pairs
                if mp_type == MemoryPortType.R:
                    sels = [mid_intf['en'] for mid_intf in mid_intf_lst]
                    ens = [mid_intf['en'] for mid_intf in mid_intf_lst]
                    addrs = [mid_intf['addr'] for mid_intf in mid_intf_lst]
                    datas = [mid_intf['data'] for mid_intf in mid_intf_lst]
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_read_en", sel=sels, one=mp_port_intf['read_en'], many=ens)
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_read_addr", sel=sels, one=mp_port_intf['addr'], many=addrs)
                    # inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name}_mux_to_mps_read_data", sel=sels, one=mp_port_intf['read_data'], many=datas)
                    # Don't actually mux read data, just send to both outputs, might not want to in the pursuit of limiting toggling
                    [self._final_gen.wire(mp_port_intf['read_data'], datas[i]) for i in range(len(datas))]

                elif mp_type == MemoryPortType.W:
                    sels = [mid_intf['en'] for mid_intf in mid_intf_lst]
                    ens = [mid_intf['en'] for mid_intf in mid_intf_lst]
                    addrs = [mid_intf['addr'] for mid_intf in mid_intf_lst]
                    datas = [mid_intf['data'] for mid_intf in mid_intf_lst]
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_write_en", sel=sels, one=mp_port_intf['write_en'], many=ens)
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_write_addr", sel=sels, one=mp_port_intf['addr'], many=addrs)
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_write_data", sel=sels, one=mp_port_intf['write_data'], many=datas)

                elif mp_type == MemoryPortType.RW:
                    sels = [mid_intf['en'] for mid_intf in mid_intf_lst]
                    # addrs = [mid_intf['addr'] for mid_intf in mid_intf_lst]

                    print("ADDRESSES")
                    print(addrs)

                    # Can build a multiplexer for the address normally
                    # inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name}_mux_to_mps_rw_addr", sel=sels, one=mp_port_intf['addr'], many=addrs)

                    # Make a mapping from each index to the direction
                    in_map = [mid_intf['direction'] == Direction.IN for mid_intf in mid_intf_lst]
                    # out_map = [1 - in_map_item for in_map_item in in_map]
                    # in_map = []
                    # out_map = []
                    # for i in range(len(mid_intf_lst)):
                    #     if mid_intf_lst[i]['direction'] == Direction.IN:
                    #         in_map.append(1)
                    #     else:
                    #         in_map.append(0)

                    # Now with this mapping, we need to build 4 streams and mux them
                    write_ens = []
                    read_ens = []
                    write_datas = []
                    write_addrs = []
                    read_addrs = []
                    # read_datas = []

                    for i, mid_intf in enumerate(mid_intf_lst):
                        data_width = mid_intf['data'].width
                        addr_width = mid_intf['addr'].width
                        if in_map[i]:
                            # Add in the signals, don't need to do anything about read data
                            write_ens.append(mid_intf['en'])
                            read_ens.append(kts.const(0, 1))
                            write_datas.append(mid_intf['data'])
                            write_addrs.append(mid_intf['addr'])
                            read_addrs.append(kts.const(0, addr_width))
                            # read_datas.append(kts.const(0, ))
                        else:
                            # Add in the signals, but if it's out, wire the data
                            write_ens.append(kts.const(0, 1))
                            read_ens.append(mid_intf['en'])
                            write_datas.append(kts.const(0, data_width))
                            write_addrs.append(kts.const(0, addr_width))
                            read_addrs.append(mid_intf['addr'])
                            self._final_gen.wire(mp_port_intf['read_data'], mid_intf['data'])

                    # Now finally mux the rest
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_rw_write_en", sel=sels, one=mp_port_intf['write_en'], many=write_ens)
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_rw_read_en", sel=sels, one=mp_port_intf['read_en'], many=read_ens)
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_rw_write_data", sel=sels, one=mp_port_intf['write_data'], many=write_datas)
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_rw_write_addr", sel=sels, one=mp_port_intf['write_addr'], many=write_addrs)
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_rw_read_addr", sel=sels, one=mp_port_intf['read_addr'], many=read_addrs)

                else:
                    raise NotImplementedError

        else:
            raise NotImplementedError

    def _connect_memintfdec_mp(self, mid_int, mp):
        # Do this in a dumb way for now but add in muxing when needed.
        mp_type = mp.get_type()
        mp_port_intf = mp.get_port_intf()
        if mp_type == MemoryPortType.R:
            self._final_gen.wire(mid_int['addr'], mp_port_intf['addr'])
            self._final_gen.wire(mid_int['data'], mp_port_intf['read_data'])
            self._final_gen.wire(mid_int['en'], mp_port_intf['read_en'])
        elif mp_type == MemoryPortType.W:
            self._final_gen.wire(mid_int['addr'], mp_port_intf['addr'])
            self._final_gen.wire(mid_int['data'], mp_port_intf['write_data'])
            self._final_gen.wire(mid_int['en'], mp_port_intf['write_en'])

    def clear_configuration(self):
        # Each node has a configuration range
        self.configuration = []
        self.config_int = 0

    def get_configuration(self):
        return self.configuration

    def get_config_base(self, node):
        node_idx = self._node_to_index[node]
        return self._config_bases[node_idx]

    def configure(self, node, bs):
        # node_config_base = self.get_config_base(node)
        print("Showing all child bases...")
        print(self._final_gen.child_cfg_bases)
        node_config_base = self._final_gen.child_cfg_bases[node]
        for reg_bound, value in bs:
            upper, lower = reg_bound
            self.configuration.append(((upper + node_config_base, lower + node_config_base), value))

    def create_config_int(self):
        # Shift and add across the board
        self.config_int = 0
        for bounds, value in self.configuration:
            upper, lower = bounds
            self.config_int = self.config_int + (value << lower)

    def get_config_int(self):
        return self.config_int

    def get_total_config_size(self):
        return self._final_gen.get_config_size()

    def gen_bitstream(self, application):
        '''Overall flow of the bitstreams is to basically go through each port and map down the information.
           There may be other information that needs to go into the configuration, but that could be in the object hierarchy
        '''

        # Need to integrate all the bitstream information
        # into one single integer/string for the verilog harness

        print(application)
        self.clear_configuration()

        # Each piece in the application is a port
        for port_num, maps in application.items():
            # Get the port and associated controllers
            port: Port = self.get_node_from_idx(port_num)

            port_vec = port.get_fw() != 1

            port_name = maps['name']
            print(f"Configuring the port named: {port_name}")
            # Get the associated controllers...
            port_config = maps['config']
            addr_map = port_config['address']
            sched_map = port_config['schedule']
            port_id, port_ag, port_sg = self.get_port_controllers(port=port)

            # Get the configuration for the Port's internal controllers if vectorized
            if port_vec:
                port_bs = port.gen_bitstream(vec_in=maps['vec_in_config'], vec_out=maps['vec_out_config'])
                self.configure(port, port_bs)

            id_bs = port_id.gen_bitstream(port_config['dimensionality'], port_config['extents'])
            ag_bs = port_ag.gen_bitstream(addr_map)
            if self.any_rv_sg:
                sg_bs = port_sg.gen_bitstream(sched_map)
            else:
                sg_bs = port_sg.gen_bitstream(sched_map)

            # Create a clear configuration
            self.configure(port_id, id_bs)
            self.configure(port_ag, ag_bs)
            self.configure(port_sg, sg_bs)

        self.create_config_int()

        print(self.get_configuration())

        return self.get_config_int()
