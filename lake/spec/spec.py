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
from lake.utils.util import connect_memoryport_storage


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

    def register_(self, comp):
        self._hw_graph.add_node(comp)
        self._hw_graph[comp]['index'] = self._num_nodes
        self._index_to_node[self._num_nodes] = comp
        self._num_nodes += 1

    def get_node_from_idx(self, idx):
        print(idx)
        print(self._index_to_node)
        assert idx in self._index_to_node
        return self._index_to_node[idx]

    def register(self, *comps):
        # self._hw_graph.add_nodes_from(comps)
        for comp in comps:
            # self.register_(comp=comp)
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

        # self.config_memory = self._final_gen.input(name=f"config_memory", width=self._config_memory_size, packed=True)
        # self.config_memory = self._final_gen.input(name=f"config_memory_size_{total_config_size}", width=total_config_size, packed=True)
        self.config_memory = self._final_gen.input(name=f"config_memory", width=total_config_size, packed=True)
        base = 0
        for node in self._hw_graph.nodes:
            cfgspc = node.get_config_space()
            for (node_config_size, signal_name_lcl) in cfgspc:
                self._final_gen.wire(self.config_memory[base + node_config_size - 1, base], signal_name_lcl)
                base += node_config_size
        # Concatenate them

    # @staticmethod
    # def connect_memoryport_storage(generator: kts.Generator, mptype: MemoryPortType = None,

    def generate_hardware(self) -> None:

        self._final_gen = kts.Generator(name=self._name, debug=False)
        # self._config_memory_size = self._final_gen.parameter('CFG_SIZE', initial_value=1)

        # self._final_gen.clk = self._final_gen.clock("clk")

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
            port_sg.gen_hardware(port_id)
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

            # step signal
            self._final_gen.wire(port_sg.ports.step, port_ag.ports.step)
            self._final_gen.wire(port_sg.ports.step, port_id.ports.step)

            # If the port is wide fetch, we can wire the SG's step and IDs signals to the port
            if port.get_fw() > 1:
                ext_intf = port.get_internal_ag_intf()
                self._final_gen.wire(ext_intf['step'], port_sg.ports.step)
                self._final_gen.wire(ext_intf['mux_sel'], port_id.ports.mux_sel)
                self._final_gen.wire(ext_intf['iterators'], port_id.ports.iterators)

            # Gen the hardware for each, assemble the signals
            assembled_port = {}
            assembled_port['dir'] = port.get_direction()
            assembled_port['data'] = port.get_mp_intf()['data']
            assembled_port['addr'] = port_ag.get_address()
            assembled_port['en'] = port_sg.get_step()

            # send signals through memintf decoder (one port to many memoryports, doing decoding based on address)
            memintf_dec = MemoryInterfaceDecoder(name=f"memintfdec_{i_}", port_type=port.get_direction(),
                                                 port_intf=assembled_port, memports=memports_)
            memintf_dec.gen_hardware()

            self._final_gen.add_child(f"memintfdec_inst_{i_}", memintf_dec)
            # After this, need to connect the decoded ports to the actual memports
            mintf_ints = memintf_dec.get_mp_intf()
            for j_, mp in enumerate(memports_):
                self._connect_memintfdec_mp(mintf_ints[j_], mp)

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

        self.lift_config_regs()
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
        node_config_base = self.get_config_base(node)
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
        return self.total_config_size

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
            sg_bs = port_sg.gen_bitstream(sched_map)

            # Create a clear configuration
            self.configure(port_id, id_bs)
            self.configure(port_ag, ag_bs)
            self.configure(port_sg, sg_bs)

        self.create_config_int()

        print(self.get_configuration())

        return self.get_config_int()
