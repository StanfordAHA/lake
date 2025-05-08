import networkx as nx
import matplotlib.pyplot as plt
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.spec.storage import Storage
from lake.spec.memory_port import MemoryPort
from lake.top.memory_controller import MemoryPort as MemoryPortMC
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
from lake.utils.util import connect_memoryport_storage, inline_multiplexer, shift_reg, round_up_to_power_of_2, lift_port
from lake.modules.rv_comparison_network import RVComparisonNetwork
from lake.spec.component import Component
from lake.modules.ready_valid_interface import RVInterface
from lake.modules.arbiter import Arbiter
from lake.spec.reg_fifo import RegFIFO
from lake.spec.hack_rv_mem_pond_bitstream import *
import math
import json


class Spec():
    """Harness for lake primitives
    """
    def __init__(self, name="lakespec", clkgate=True,
                 config_passthru=False,
                 opt_rv=False, remote_storage=False,
                 run_flush_pass=True,
                 comply_17=False) -> None:
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
        self.runtime = Runtime.STATIC
        self.rv_comparison_network = None
        self.num_ports = 0
        self.num_in_ports = 0
        self.num_out_ports = 0
        self.in_ports = []
        self.out_ports = []
        self.num_mem_ports = 0
        self.fw_max = 1
        self.clk_gate = clkgate
        self.config_passthru = config_passthru
        self.opt_rv = opt_rv
        self.remote_storage = remote_storage
        self.mc_ports = [[None]]
        self.run_flush_pass = run_flush_pass
        self._final_gen = Component(name=self._name)
        # When complying with 17, we add an extra bit for integration with CGRA
        self.comply_17 = comply_17

    def set_name(self, name):
        self._name = name

    def get_internal_generator(self):
        return self._final_gen.internal_generator

    def get_generator(self):
        # print(self._final_gen.child_generator())
        # exit()
        return self._final_gen

    def get_memory_ports_mc(self):
        '''
        Return the memory ports for the MemoryController
        - This is for the MemoryController to know which ports are connected to it (for CGRA integration)
        '''
        # When generating the hardware, this will be done automatically -
        # for now, we can just return the memory ports (for mc)
        return self.mc_ports

    def annotate_liftable_ports(self):
        '''
        Annotating the liftable ports for the CoreCombiner will occur during hardware generation...
        '''
        pass

    def register_(self, comp):
        self._hw_graph.add_node(comp)
        self._hw_graph[comp]['index'] = self._num_nodes
        self._index_to_node[self._num_nodes] = comp
        self._num_nodes += 1

    def get_num_ports(self):
        return self.num_ports

    def get_num_in_ports(self):
        return self.num_in_ports

    def get_num_out_ports(self):
        return self.num_out_ports

    def get_in_ports(self):
        return self.in_ports

    def get_out_ports(self):
        return self.out_ports

    def get_node_from_idx(self, idx, verbose=False):
        if verbose:
            print(idx)
            print(self._index_to_node)
        assert idx in self._index_to_node
        return self._index_to_node[idx]

    def convert_mp_to_mcmp(self, hard_port: dict, soft_port: MemoryPortMC):
        # For now, just copy the hard port to the soft port
        # and connect the signals

        # First, get the type of the port
        port_type = soft_port.get_port_type()

        soft_port_interface = soft_port.get_port_interface()

        print(f"hard port: {hard_port}")
        print(f"soft port (MC): {soft_port_interface}")

        if port_type == MemoryPortType.READ:
            soft_port_interface['data_out'] = hard_port['read_data']
            soft_port_interface['read_addr'] = hard_port['addr']
            soft_port_interface['read_enable'] = hard_port['read_en']
        elif port_type == MemoryPortType.WRITE:
            soft_port_interface['data_in'] = hard_port['write_data']
            soft_port_interface['write_addr'] = hard_port['addr']
            soft_port_interface['write_enable'] = hard_port['write_en']
            # Manage the possibility of clear...
            if 'clear' in hard_port:
                print("Clear in convert to memory port...")
                soft_port_interface['clear'] = hard_port['clear']
        elif port_type == MemoryPortType.READWRITE:
            soft_port_interface['data_in'] = hard_port['write_data']
            soft_port_interface['data_out'] = hard_port['read_data']
            soft_port_interface['read_addr'] = hard_port['addr']
            soft_port_interface['read_enable'] = hard_port['read_en']
            soft_port_interface['write_addr'] = hard_port['addr']
            soft_port_interface['write_enable'] = hard_port['write_en']
        else:
            raise NotImplementedError(f"Port Type {port_type} not supported...")

    def connect_memoryport_mc_interface(self, memory_port: MemoryPort, bank_no=(0, 0)):

        # Get the interface of the memory port
        # and basically copy these to the interface of the module,
        # create a MemoryPortMC and also add to the mc_ports list
        mp_intf = memory_port.get_storage_intf()
        print(mp_intf)
        port_type = memory_port.get_type()

        new_intf = {}

        for port_name, port in mp_intf.items():
            new_port = lift_port(child_gen=memory_port, parent_gen=self._final_gen,
                                 child_port=port, suffix=f"_{memory_port.name}",)
            new_intf[port_name] = new_port

        print(new_intf)
        mc_port = MemoryPortMC(port_type)
        # Now add the hard ports to the soft mc port for integration in CGRA
        # mc_port_intf = mc_port.get_port_interface()
        self.convert_mp_to_mcmp(hard_port=new_intf, soft_port=mc_port)
        mc_port.annotate_port_signals()

        # Default one storage for now...
        self.mc_ports[bank_no[0]][bank_no[1]] = mc_port

        print("Done lifting...")

    def register(self, *comps):
        # self._hw_graph.add_nodes_from(comps)
        for comp in comps:
            # self.register_(comp=comp)
            if isinstance(comp, ScheduleGenerator) and comp.get_rv():
                self.any_rv_sg = True
                self.runtime = Runtime.DYNAMIC
            elif isinstance(comp, MemoryPort):
                comp: MemoryPort
                self.num_mem_ports += 1
            elif isinstance(comp, Port):
                comp: Port
                self.num_ports += 1
                if comp.get_fw() > self.fw_max:
                    self.fw_max = comp.get_fw()
                pdir = comp.get_direction()
                if pdir == Direction.IN:
                    self.num_in_ports += 1
                    self.in_ports.append(comp)
                elif pdir == Direction.OUT:
                    self.num_out_ports += 1
                    self.out_ports.append(comp)
                else:
                    raise NotImplementedError(f"Port Direction {pdir} not supported...")

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

    def visualize_graph(self, gname="spec", outdir=".") -> None:
        plt.figure(figsize=(8, 8))
        nx.draw(self._hw_graph, with_labels=True, font_weight='bold')
        outpath = os.path.join(outdir, f"{gname}.png")
        plt.savefig(outpath, dpi=300)

    def get_nodes(self, node_type) -> list:
        ret_list = list()
        for node in self._hw_graph.nodes():
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

        # self._final_gen = Component(name=self._name)
        # Before we go into the port loop, if any of the schedule generators is dynamic (vs. static), we need to know this and collect
        # information before genning the hardware

        # Just instantiate one comparison thing for now...
        if self.any_rv_sg:
            # In later stage of spec hw gen will add each read and write port to this...
            self.rv_comparison_network = RVComparisonNetwork(name='rv_comparison_network_this_spec')

        self.hw_attr = {}
        self.hw_attr['clk'] = self._final_gen.get_clock()
        self.hw_attr['rst_n'] = self._final_gen.get_reset()
        self.hw_attr['flush'] = self._final_gen.get_flush()
        self.hw_attr['clk_en'] = self._final_gen.get_clock_enable()

        # First generate the storages based on the ports connected to them and their capacities
        storage_nodes = self.get_nodes(Storage)

        self.mc_ports = []
        for j_, storage_node in enumerate(storage_nodes):
            self.mc_ports.append([])

            storage_node: Storage
            # get MemoryPorts
            memoryports = list(nx.neighbors(self._hw_graph, storage_node))

            # Only build the hardware for the storage if it is not remote
            if self.remote_storage is False:

                storage_node.gen_hardware(pos_reset=False, memory_ports=memoryports)
                # memoryports = nx.neighbors(self._hw_graph, storage_node)
                # Now we have the storage generated, want to generate the memoryports hardware which will be simply
                # passthru of the port currently...
                # Now generate a storage element based on all of these ports and add them to the final generator
                self._final_gen.add_child("storage", storage_node, clk=self.hw_attr['clk'],
                                          rst_n=self.hw_attr['rst_n'], clk_en=self.hw_attr['clk_en'], flush=self.hw_attr['flush'])

            print("IDK")
            strg_intfs = storage_node.get_memport_intfs()
            print(strg_intfs)

            # Build memory ports
            memoryports = nx.neighbors(self._hw_graph, storage_node)
            for i_, mp in enumerate(memoryports):
                self.mc_ports[j_].append(None)
                mp: MemoryPort
                mp.gen_hardware(pos_reset=False, storage_node=storage_node)
                self._final_gen.add_child(f"memoryport_{i_}_storage_{j_}", mp,
                                          clk=self.hw_attr['clk'],
                                          rst_n=self.hw_attr['rst_n'],
                                          clk_en=self.hw_attr['clk_en'], flush=self.hw_attr['flush'])
                # self._connect_memoryport_storage(mptype=mp.get_type(), memport_intf=mp.get_storage_intf(), strg_intf=strg_intfs[i_])
                if self.remote_storage is False:
                    connect_memoryport_storage(self._final_gen, mptype=mp.get_type(), memport_intf=mp.get_storage_intf(), strg_intf=strg_intfs[i_])
                else:
                    self.connect_memoryport_mc_interface(mp, bank_no=(j_, i_))
                # Connected the memory ports to the storage

        # Now that we have generated the memory ports and storage, we can realize
        # the ports and supporting hardware

        # port_nodes = self.get_nodes(Port)
        # Ensure all input ports come before output ports...
        port_nodes = self.get_in_ports() + self.get_out_ports()
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

            # If we are using the optimized implementation, it is crucial
            # make the address generator wider than the main memory (use word-level addressing)
            if self.opt_rv:
                width_mult = port.get_fw()
                port_ag.set_width_mult(width_mult)

            port_ag.gen_hardware(memports_, port_id)
            port.set_port_ag_width(port_ag.get_address_width())

            if self.any_rv_sg:
                # We need to include some information about the other ports when building schedule generator
                if port_direction == Direction.IN:
                    port_sg.gen_hardware(id=port_id, num_comparisons=self.num_out_ports)
                elif port_direction == Direction.OUT:
                    port_sg.gen_hardware(id=port_id, num_comparisons=self.num_in_ports)
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
                                      rst_n=self.hw_attr['rst_n'],
                                      clk_en=self.hw_attr['clk_en'], flush=self.hw_attr['flush'])
            # Connect the ag/sg/id together
            self._final_gen.add_child(f"port_id_{i_}", port_id,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'],
                                      clk_en=self.hw_attr['clk_en'], flush=self.hw_attr['flush'])
            self._final_gen.add_child(f"port_ag_{i_}", port_ag,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'],
                                      clk_en=self.hw_attr['clk_en'], flush=self.hw_attr['flush'])
            self._final_gen.add_child(f"port_sg_{i_}", port_sg,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'],
                                      clk_en=self.hw_attr['clk_en'], flush=self.hw_attr['flush'])

            self._final_gen.wire(port_id.ports.mux_sel, port_ag.ports.mux_sel)
            self._final_gen.wire(port_id.ports.restart, port_ag.ports.restart)
            self._final_gen.wire(port_id.ports.finished, port_ag.ports.finished)
            self._final_gen.wire(port_id.ports.iterators, port_ag.ports.iterators)

            self._final_gen.wire(port_id.ports.mux_sel, port_sg.ports.mux_sel)
            self._final_gen.wire(port_id.ports.restart, port_sg.ports.restart)
            self._final_gen.wire(port_id.ports.finished, port_sg.ports.finished)
            self._final_gen.wire(port_id.ports.iterators, port_sg.ports.iterators)

            # Send through the extents to sg if there is RV
            if self.any_rv_sg:
                self._final_gen.wire(port_id.ports.extents_out, port_sg.ports.extents)

            # Gen the hardware for each, assemble the signals
            assembled_port = {}
            assembled_port['dir'] = port.get_direction()
            assembled_port['data'] = port.get_mp_intf()['data']
            # assembled_port['addr'] = port_ag.get_address()
            assembled_port['addr'] = port_ag.get_memory_address()
            # assembled_port['en'] = quali_step
            # assembled_port['en'] = port_sg.get_step()

            # send signals through memintf decoder (one port to many memoryports, doing decoding based on address)
            memintf_dec = MemoryInterfaceDecoder(name=f"memintfdec_{i_}", port_type=port.get_direction(),
                                                 port_intf=assembled_port, memports=memports_,
                                                 runtime=self.runtime, opt_rv=self.opt_rv)
            memintf_dec.gen_hardware()

            self._final_gen.add_child(f"memintfdec_inst_{i_}", memintf_dec,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'],
                                      clk_en=self.hw_attr['clk_en'], flush=self.hw_attr['flush'])
            # After this, need to connect the decoded ports to the actual memports
            mintf_ints = memintf_dec.get_mp_intf()
            for j_, mp in enumerate(memports_):
                self.register_mid_w_mp(mid=mintf_ints[j_], mp=mp)
                # self._connect_memintfdec_mp(mintf_ints[j_], mp)

            # Have to qualify the step after the MID is created so we can get the grant from that
            quali_step = port_sg.ports.step
            # Here we need to qualify the port sg's step before sending it into the memory port, ID, SG
            if self.any_rv_sg:
                # If it is an IN Port, we need to qualify the step with the incoming data being valid
                # as well as the grant for the MemoryPort's arbitration
                # In this case, the step is the req line to the arbiter and the ready line to the data
                if port_direction == Direction.IN:
                    # quali_step = self.qualify_step(port_sg, Direction.IN)
                    port_valid = port.get_mp_intf()['valid']
                    sg_step = port_sg.ports.step
                    mid_grant = memintf_dec.get_p_intf()['grant']
                    # The enable to mid is valid + step
                    quali_step = sg_step & port_valid & memintf_dec.ports.resource_ready
                    # The grant is the ready/final step to ID, AG, ready
                    self._final_gen.wire(port.get_mp_intf()['ready'], mid_grant)

                    if port.get_fw() > 1 and self.opt_rv:
                        self._final_gen.wire(port_ag.ports.step, port.ports.sg_step_out)
                        self._final_gen.wire(port_id.ports.step, port.ports.sg_step_out)
                        self._final_gen.wire(port_id.ports.finished, port.ports.finished)
                    else:
                        self._final_gen.wire(port_ag.ports.step, mid_grant)
                        self._final_gen.wire(port_id.ports.step, mid_grant)
                    # Wire the ID step
                # If it is an OUT Port, we need to qualify the step with the grant line from the arbitration and the
                # downstream ready (from the Port I guess)
                elif port_direction == Direction.OUT:

                    port_ready = port.get_mp_intf()['ready']
                    sg_step = port_sg.ports.step
                    # Don't give grant unless there is a ready from port...
                    mid_grant = memintf_dec.get_p_intf()['grant'] & port_ready
                    # The enable to mid is memintf decoder resource available ready + step
                    quali_step = sg_step & memintf_dec.ports.resource_ready
                    # quali_step = sg_step & port_ready
                    # The grant is the ready/final step to ID, AG, ready
                    # self._final_gen.wire(port.get_mp_intf()['valid'], mid_grant)
                    self._final_gen.wire(port.get_mp_intf()['valid'], memintf_dec.ports.data_valid)
                    # The ready comes out of the memintf decoder
                    self._final_gen.wire(memintf_dec.ports.data_ready, port_ready)

                    # If wide and optimizing rv, we should just give them the port's step out
                    if port.get_fw() > 1 and self.opt_rv:
                        self._final_gen.wire(port_ag.ports.step, port.ports.sg_step_out)
                        self._final_gen.wire(port_id.ports.step, port.ports.sg_step_out)
                        self._final_gen.wire(port_id.ports.finished, port.ports.finished)
                        self._final_gen.wire(mid_grant, port.ports.grant)
                    else:
                        self._final_gen.wire(port_ag.ports.step, mid_grant)
                        self._final_gen.wire(port_id.ports.step, mid_grant)

                else:
                    raise NotImplementedError(f"Only support {Direction.IN} and {Direction.OUT}")

                # Now that we have the qualified steps to plug in everywhere, we also need to handle comparison hardware
            else:
                self._final_gen.wire(port_sg.ports.step, port_ag.ports.step)
                # self._final_gen.wire(port_sg.ports.step, port_id.ports.step)
                self._final_gen.wire(port_sg.ports.step, port_id.ports.step)

                # Need to also wire the readys
                if port_direction == Direction.IN:
                    # self._final_gen.wire(port.get_mp_intf()['ready'], kts.const(1, 1))
                    self._final_gen.wire(port.get_mp_intf()['ready'], port_sg.ports.step)
                elif port_direction == Direction.OUT:
                    self._final_gen.wire(memintf_dec.ports.data_ready, kts.const(1, 1))
                    # Actually pipe the sg's step through in
                    # self._final_gen.wire(port.get_mp_intf()['valid'], kts.const(1, 1))
                    # self._final_gen.wire(port.get_mp_intf()['valid'], port_sg.ports.step)
                    self._final_gen.wire(port.get_mp_intf()['valid'], memintf_dec.ports.data_valid)

            # If the port is wide fetch, we can wire the SG's step and IDs signals to the port
            if port.get_fw() > 1:

                # Fill in the optimization here...
                if self.any_rv_sg and self.opt_rv and port_direction == Direction.OUT:

                    # For this port, we want to give it the step from the SG and the addr from the AG
                    # quali_step = sg_step &memintf_dec.ports.resource_ready
                    # quali_step = sg_step & memintf_dec.ports.resource_ready

                    self._final_gen.wire(port_sg.ports.step, port.ports.sg_step_in)
                    self._final_gen.wire(port_ag.get_address(), port.ports.addr_in)
                    assembled_port['addr'] = port.ports.addr_out
                    # quali_step is now the ready of the memintf_dec with the output step of the Port
                    quali_step = port.ports.read_memory_out & memintf_dec.ports.resource_ready

                elif port_direction == Direction.IN and self.any_rv_sg and self.opt_rv:
                    self._final_gen.wire(port_sg.ports.step, port.ports.sg_step_in)
                    self._final_gen.wire(port_ag.get_address(), port.ports.addr_in)
                    assembled_port['addr'] = port.ports.addr_out
                    # quali_step is now the ready of the memintf_dec with the output step of the Port
                    # quali_step = port.ports.write_memory_out & memintf_dec.ports.resource_ready
                    # quali_step = port.ports.write_memory_out & memintf_dec.ports.resource_ready
                    quali_step = port.get_mp_intf()['valid'] & memintf_dec.ports.resource_ready

                else:
                    ext_intf = port.get_internal_ag_intf()

                    if port_direction == Direction.IN:
                        # For a write Port, just directly connect everything
                        self._final_gen.wire(ext_intf['step'], quali_step)
                        self._final_gen.wire(ext_intf['mux_sel'], port_id.ports.mux_sel)
                        self._final_gen.wire(ext_intf['iterators'], port_id.ports.iterators)
                        self._final_gen.wire(ext_intf['finished'], port_id.ports.finished)
                        self._final_gen.wire(ext_intf['restart'], port_id.ports.restart)
                        if self.any_rv_sg:
                            self._final_gen.wire(ext_intf['extents'], port_id.ports.extents_out)

                    elif port_direction == Direction.OUT:
                        # For a read Port, slightly more complicated - need to actually have the delayed
                        # version of everything (but can handle that within the Port...)

                        delay = memintf_dec.get_delay()

                        # These should be shift regs in static, fifo in ready/valid
                        if self.any_rv_sg:

                            # pass
                            rupp2 = round_up_to_power_of_2(2 + delay)
                            # reg_fifo = RegFIFO(port_id.ports.mux_sel.width, port_id.ports.mux_sel.size[0], rupp2, almost_full_diff=delay + 1)
                            # self._final_gen.add_child(f"reg_fifo_port_{i_}_mux_sel",
                            #                 reg_fifo,
                            #                 clk=self.hw_attr['clk'],
                            #                 rst_n=self.hw_attr['rst_n'],
                            #                 # clk_en=self._clk_en,
                            #                 clk_en=kts.const(1, 1),
                            #                 push=quali_step,
                            #                 pop=ext_intf['mux_sel'].get_ready(),
                            #                 data_in=port_id.ports.mux_sel,
                            #                 data_out=ext_intf['mux_sel'].get_port())
                            # self._final_gen.wire(ext_intf['mux_sel'].get_valid(), ~reg_fifo.ports.empty)

                            # reg_fifo = RegFIFO(port_id.ports.iterators.width, port_id.ports.iterators.size[0], rupp2, almost_full_diff=delay + 1)
                            # self._final_gen.add_child(f"reg_fifo_port_{i_}_iterators",
                            #                 reg_fifo,
                            #                 clk=self.hw_attr['clk'],
                            #                 rst_n=self.hw_attr['rst_n'],
                            #                 # clk_en=self._clk_en,
                            #                 clk_en=kts.const(1, 1),
                            #                 push=quali_step,
                            #                 pop=ext_intf['iterators'].get_ready(),
                            #                 data_in=port_id.ports.iterators,
                            #                 data_out=ext_intf['iterators'].get_port())
                            # self._final_gen.wire(ext_intf['iterators'].get_valid(), ~reg_fifo.ports.empty)

                            # reg_fifo = RegFIFO(port_id.ports.restart.width, port_id.ports.restart.size[0], rupp2, almost_full_diff=delay + 1)
                            # self._final_gen.add_child(f"reg_fifo_port_{i_}_restart",
                            #                 reg_fifo,
                            #                 clk=self.hw_attr['clk'],
                            #                 rst_n=self.hw_attr['rst_n'],
                            #                 # clk_en=self._clk_en,
                            #                 clk_en=kts.const(1, 1),
                            #                 push=quali_step,
                            #                 pop=ext_intf['restart'].get_ready(),
                            #                 data_in=port_id.ports.restart,
                            #                 data_out=ext_intf['restart'].get_port())
                            # self._final_gen.wire(ext_intf['restart'].get_valid(), ~reg_fifo.ports.empty)

                            # reg_fifo = RegFIFO(port_id.ports.extents_out.width, port_id.ports.extents_out.size[0], rupp2, almost_full_diff=delay + 1)
                            # self._final_gen.add_child(f"reg_fifo_port_{i_}_extents",
                            #                 reg_fifo,
                            #                 clk=self.hw_attr['clk'],
                            #                 rst_n=self.hw_attr['rst_n'],
                            #                 # clk_en=self._clk_en,
                            #                 clk_en=kts.const(1, 1),
                            #                 push=quali_step,
                            #                 pop=ext_intf['extents'].get_ready(),
                            #                 data_in=port_id.ports.extents_out,
                            #                 data_out=ext_intf['extents'].get_port())
                            # self._final_gen.wire(ext_intf['extents'].get_valid(), ~reg_fifo.ports.empty)
                            # self._final_gen.wire(ext_intf['extents'], port_id.ports.extents_out)

                            # delay_mux_sel = shift_reg(self._final_gen, port_id.ports.mux_sel, chain_depth=delay, name=f"shreg_mux_sel_port_{i_}")
                            # delay_iterators = shift_reg(self._final_gen, port_id.ports.iterators, chain_depth=delay, name=f"shreg_iterators_port_{i_}")
                            # delay_restart = shift_reg(self._final_gen, port_id.ports.restart, chain_depth=delay, name=f"shreg_restart_port_{i_}")

                            # self._final_gen.wire(ext_intf['step'], shreg_step)
                            # self._final_gen.wire(ext_intf['mux_sel'], delay_mux_sel)
                            # self._final_gen.wire(ext_intf['iterators'], delay_iterators)
                            # self._final_gen.wire(ext_intf['restart'], delay_restart)
                            # self._final_gen.wire(ext_intf['extents'], port_id.ports.extents_out)

                        else:
                            shreg_step = shift_reg(self._final_gen, quali_step, chain_depth=delay, name=f"shreg_step_port_{i_}")
                            delay_mux_sel = shift_reg(self._final_gen, port_id.ports.mux_sel, chain_depth=delay, name=f"shreg_mux_sel_port_{i_}")
                            delay_iterators = shift_reg(self._final_gen, port_id.ports.iterators, chain_depth=delay, name=f"shreg_iterators_port_{i_}")
                            delay_restart = shift_reg(self._final_gen, port_id.ports.restart, chain_depth=delay, name=f"shreg_restart_port_{i_}")

                            self._final_gen.wire(ext_intf['step'], shreg_step)
                            self._final_gen.wire(ext_intf['mux_sel'], delay_mux_sel)
                            self._final_gen.wire(ext_intf['iterators'], delay_iterators)
                            self._final_gen.wire(ext_intf['restart'], delay_restart)

                        # if self.any_rv_sg:

            memintf_dec_p_intf = memintf_dec.get_p_intf()
            self._final_gen.wire(assembled_port['data'], memintf_dec_p_intf['data'])
            self._final_gen.wire(assembled_port['addr'], memintf_dec_p_intf['addr'])
            # self._final_gen.wire(assembled_port['en'], memintf_dec_p_intf['en'])
            self._final_gen.wire(quali_step, memintf_dec_p_intf['en'])

            # Now add the port interfaces to the module
            # Can annotate these (and potentially add FIFOs if needed (since they will be shared anyway, might as well))
            ub_intf = port.get_ub_intf()

            compliance_adjustment = 0
            if self.comply_17:
                compliance_adjustment = 1

            if port.get_direction() == Direction.IN:
                if self.any_rv_sg:
                    if port.get_dangling():
                        self._final_gen.wire(ub_intf['valid'], 1)
                        self._final_gen.wire(ub_intf['data'], 0)
                    else:
                        p_temp_rv = self._final_gen.rvinput(name=f"port_{i_}", width=ub_intf['data'].width + compliance_adjustment, packed=True)
                        p_temp = p_temp_rv.get_port()
                        p_temp_valid = p_temp_rv.get_valid()
                        p_temp_ready = p_temp_rv.get_ready()
                        self._final_gen.wire(p_temp_valid, ub_intf['valid'])
                        self._final_gen.wire(p_temp_ready, ub_intf['ready'])
                        # Wire only the relevant part - regardless of compliance adjustment
                        self._final_gen.wire(ub_intf['data'], p_temp[ub_intf['data'].width - 1, 0])
                else:
                    if port.get_dangling():
                        self._final_gen.wire(ub_intf['valid'], 1)
                        self._final_gen.wire(ub_intf['data'], 0)
                    else:
                        p_temp = self._final_gen.input(f"port_{i_}", width=ub_intf['data'].width + compliance_adjustment)
                        p_temp_valid = self._final_gen.input(f"port_{i_}_valid", 1)
                        p_temp_ready = self._final_gen.output(f"port_{i_}_ready", 1)
                        # self._final_gen.wire(p_temp_ready, kts.const(1, 1))
                        self._final_gen.wire(p_temp_ready, ub_intf['ready'])
                        self._final_gen.wire(ub_intf['valid'], p_temp_valid)
                        # Wire only the relevant part - regardless of compliance adjustment
                        self._final_gen.wire(ub_intf['data'], p_temp[ub_intf['data'].width - 1, 0])
                if self.remote_storage is True:
                    # Annotate signals with ControlSignalAttr for CoreCombiner...'
                    p_temp.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
                    p_temp_valid.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
                    p_temp_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

            elif port.get_direction() == Direction.OUT:
                if self.any_rv_sg:
                    p_temp_rv = self._final_gen.rvoutput(name=f"port_{i_}", width=ub_intf['data'].width + compliance_adjustment, packed=True)
                    p_temp = p_temp_rv.get_port()
                    p_temp_valid = p_temp_rv.get_valid()
                    p_temp_ready = p_temp_rv.get_ready()
                    self._final_gen.wire(p_temp_valid, ub_intf['valid'])
                    self._final_gen.wire(p_temp_ready, ub_intf['ready'])
                else:
                    p_temp = self._final_gen.output(f"port_{i_}", width=ub_intf['data'].width + compliance_adjustment)
                    p_temp_valid = self._final_gen.output(f"port_{i_}_valid", 1)
                    p_temp_ready = self._final_gen.input(f"port_{i_}_ready", 1)
                    # self._final_gen.wire(p_temp_valid, kts.const(1, 1))
                    self._final_gen.wire(p_temp_valid, ub_intf['valid'])
                    self._final_gen.wire(ub_intf['ready'], p_temp_ready)

                # If adjusting for compliance adjustment, drive 0 otherwise...
                self._final_gen.wire(p_temp[ub_intf['data'].width - 1, 0], ub_intf['data'])
                if self.comply_17:
                    self._final_gen.wire(p_temp[ub_intf['data'].width], kts.const(0, 1))

                if self.remote_storage is True:
                    # Annotate signals with ControlSignalAttr for CoreCombiner...'
                    p_temp.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
                    p_temp_valid.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
                    p_temp_ready.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

            else:
                raise NotImplementedError

            # self._final_gen.wire(p_temp, ub_intf['data'])

        # Now can build all the muxing in between the memintf and the mps
        self.build_mp_p()

        # Can now build the comparison network if it's there...
        if self.any_rv_sg:
            self.rv_comparison_network.gen_hardware()
            self._final_gen.add_child('rv_comp_network_top_spec', self.rv_comparison_network,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'],
                                      clk_en=self.hw_attr['clk_en'], flush=self.hw_attr['flush'])
            rv_comp_conns = self.rv_comparison_network.get_connections()
            for conn_tuple in rv_comp_conns:
                p1, p2 = conn_tuple
                self._final_gen.wire(p1, p2)

        # self.lift_config_regs()
        print("building spec cfg memory input")
        # Can also choose to clock gate the config memory...
        # self._final_gen._assemble_cfg_memory_input(harden_storage=True, clkgate=self.clk_gate)
        self._final_gen._assemble_cfg_memory_input(harden_storage=True, clkgate=self.clk_gate,
                                                   config_passthru=self.config_passthru)
        self.add_flush()
        # Optionally add clock gate automatically
        # if self.clk_gate:

    def get_information(self, output_dir):
        # Need to emit # config bits, # input ports, # output ports
        all_info = {
            "config_size": self.get_total_config_size(),
            "input_ports": self.num_in_ports,
            "output_ports": self.num_out_ports,
            "fw": self.fw_max,
            "memory_ports": self.num_mem_ports
        }

        outfile = os.path.join(output_dir, "info.json")
        with open(outfile, 'w') as json_file:
            json.dump(all_info, json_file, indent=4)

    def remove_config_memory_flush(self, filename):

        all_contents = None
        with open(filename, 'r') as f_:
            all_contents = f_.readlines()

        at_cfg_mod = False
        for idx, line in enumerate(all_contents):

            if at_cfg_mod is True and "else if (flush)" in line:
                all_contents.pop(idx)
                all_contents.pop(idx)
                all_contents.pop(idx)
                break

            if 'module config_memory_' in line:
                at_cfg_mod = True

        with open(filename, 'w+') as f_:
            f_.writelines(all_contents)

    def get_verilog(self, output_dir, get_info=True, verbose=False):
        fn_ = f"{self._name}.sv"
        full_path = os.path.join(output_dir, fn_)

        kts.verilog(self._final_gen, filename=full_path,
                    optimize_if=False)
        if verbose:
            print(f"remove flush use in config memory")
        self.remove_config_memory_flush(filename=full_path)

        if get_info is True:
            self.get_information(output_dir=output_dir)

    def add_flush(self):
        return
        # self._final_gen.add_attribute("sync-reset=flush")
        # if self.run_flush_pass is True:
        #     kts.passes.auto_insert_sync_reset(self._final_gen.internal_generator)
        # flush_port = self._final_gen.internal_generator.get_port("flush")
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

    def insert_arbiter(self, reqs, grants, arb_name):
        '''This function adds an arbiter, just pass in the req lines and it will produce an object
            with both a reqs and grants field
        '''
        tmp_arb = Arbiter(ins=len(reqs))
        self._final_gen.add_child(arb_name, tmp_arb,
                                  clk=self.hw_attr['clk'],
                                  rst_n=self.hw_attr['rst_n'],
                                  clk_en=self.hw_attr['clk_en'], flush=self.hw_attr['flush'],
                                  resource_ready=kts.const(1, 1))

        tmp_arb_reqs = tmp_arb.get_reqs()
        tmp_arb_grants = tmp_arb.get_grants()
        for i in range(len(reqs)):
            self._final_gen.wire(tmp_arb_reqs[i], reqs[i])
            self._final_gen.wire(tmp_arb_grants[i], grants[i])

        return tmp_arb

    def build_mp_p(self):
        ''' We have to now build the arbitration and muxes between the Ports and
            their shared MemoryPorts - if `static` is `True`, arbitration will just be simple priority
            encoding as that won't matter since no two controllers will access the same
            resource on the same cycle
        '''

        self._all_mp_arbiters = {}
        if True:
            # if not self.any_rv_sg:
            for mp, mid_intf_lst in self.mp_to_mid.items():
                # We have a memory port and a set of connections to go to it - build a mux for
                # addr, data, en
                mp: MemoryPort
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

                    # Can just interpret sels here and treat them as reqs, then feed the grants back to the mux
                    # arbiter per mp
                    if self.any_rv_sg:
                        sels = [mid_intf['en'] for mid_intf in mid_intf_lst]
                        # grants = self._final_gen.var(name=f"{mp.get_name()}_arbiter_grants", width=len(mid_intf_lst))
                        grants = [mid_intf['grant'] for mid_intf in mid_intf_lst]
                        tmp_arb = self.insert_arbiter(reqs=sels, grants=grants,
                                                      arb_name=f"{mp.get_name()}_arbiter")
                        self._all_mp_arbiters[mp] = {'arbiter': tmp_arb,
                                                     'reqs': sels,
                                                     'grants': grants}
                        sels = tmp_arb.get_grants()
                        # sels = grants
                    else:
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

                    # Can just interpret sels here and treat them as reqs, then feed the grants back to the mux
                    # arbiter per mp
                    if self.any_rv_sg:
                        sels = [mid_intf['en'] for mid_intf in mid_intf_lst]
                        # grants = self._final_gen.var(name=f"{mp.get_name()}_arbiter_grants", width=len(mid_intf_lst))
                        grants = [mid_intf['grant'] for mid_intf in mid_intf_lst]
                        tmp_arb = self.insert_arbiter(reqs=sels, grants=grants,
                                                      arb_name=f"{mp.get_name()}_arbiter")
                        self._all_mp_arbiters[mp] = {'arbiter': tmp_arb,
                                                     'reqs': sels,
                                                     'grants': grants}
                        sels = tmp_arb.get_grants()
                        # sels = grants
                    else:
                        sels = [mid_intf['en'] for mid_intf in mid_intf_lst]

                    # sels = [mid_intf['en'] for mid_intf in mid_intf_lst]
                    ens = [mid_intf['en'] for mid_intf in mid_intf_lst]
                    addrs = [mid_intf['addr'] for mid_intf in mid_intf_lst]
                    datas = [mid_intf['data'] for mid_intf in mid_intf_lst]
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_write_en", sel=sels, one=mp_port_intf['write_en'], many=ens)
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_write_addr", sel=sels, one=mp_port_intf['addr'], many=addrs)
                    inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_write_data", sel=sels, one=mp_port_intf['write_data'], many=datas)
                    if mp.get_clear_mem():
                        clears = [mid_intf['clear'] for mid_intf in mid_intf_lst]
                        inline_multiplexer(generator=self._final_gen, name=f"{mp.get_name()}_mux_to_mps_clear", sel=sels, one=mp_port_intf['clear'], many=clears)

                elif mp_type == MemoryPortType.RW:
                    # sels = [mid_intf['en'] for mid_intf in mid_intf_lst]
                    # addrs = [mid_intf['addr'] for mid_intf in mid_intf_lst]

                    # Can just interpret sels here and treat them as reqs, then feed the grants back to the mux
                    # arbiter per mp
                    if self.any_rv_sg:
                        sels = [mid_intf['en'] for mid_intf in mid_intf_lst]
                        # grants = self._final_gen.var(name=f"{mp.get_name()}_arbiter_grants", width=len(mid_intf_lst))
                        grants = [mid_intf['grant'] for mid_intf in mid_intf_lst]
                        tmp_arb = self.insert_arbiter(reqs=sels, grants=grants,
                                                      arb_name=f"{mp.get_name()}_arbiter")
                        self._all_mp_arbiters[mp] = {'arbiter': tmp_arb,
                                                     'reqs': sels,
                                                     'grants': grants}
                        # sels = grants
                        sels = tmp_arb.get_grants()
                    else:
                        sels = [mid_intf['en'] for mid_intf in mid_intf_lst]

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
        # Now to build arbiter if dynamic - the ens are actually now the req, need to send back grant
        else:
            raise NotImplementedError

    def _connect_memintfdec_mp(self, mid_int, mp: MemoryPort):
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
            if mp.get_clear_mem():
                self._final_gen.wire(mid_int['clear'], mp_port_intf['clear'])

    def clear_configuration(self):
        # Each node has a configuration range
        self.configuration = []
        self.config_int = 0

    def get_configuration(self):
        return self.configuration

    def get_config_base(self, node):
        node_idx = self._node_to_index[node]
        return self._config_bases[node_idx]

    def configure(self, node, bs, verbose=False):
        # node_config_base = self.get_config_base(node)
        if verbose:
            print("Showing all child bases...")
            for gen__, base in self._final_gen.child_cfg_bases.items():
                print(f"{base} : {gen__.name} : {gen__.instance_name}")
        node_config_base = self._final_gen.child_cfg_bases[node]
        for reg_bound, value in bs:
            upper, lower = reg_bound
            if verbose:
                print(f"{upper + node_config_base},{lower + node_config_base} = {value}")
            self.configuration.append(((upper + node_config_base, lower + node_config_base), value))

    def create_config_int(self):
        # Shift and add across the board
        self.config_int = 0
        for bounds, value in self.configuration:
            upper, lower = bounds
            # Create binary number
            size_mask = upper - lower + 1
            # Make sure to trim it!
            bmask = int(math.pow(2, size_mask)) - 1
            self.config_int |= (value & bmask) << lower

    def get_config_int(self):
        return self.config_int

    def get_total_config_size(self):
        return self._final_gen.get_config_size()

    def port_name_to_int(self, pname):
        if 'port_w' in pname:
            return int(pname.split('port_w')[1])
        elif 'port_r' in pname:
            return int(pname.split('port_r')[1]) + self.get_num_in_ports()
        else:
            raise ValueError

    def get_base_port_config(self, port_name):
        assert "port" in port_name
        base_port_config = {}
        port_int = self.port_name_to_int(port_name)
        base_port_config['name'] = port_name
        if "port_w" in port_name:
            base_port_config['type'] = Direction.IN
        elif "port_r" in port_name:
            base_port_config['type'] = Direction.OUT
        else:
            raise ValueError

        base_port_config['config'] = {}

        return base_port_config

    def get_conv_2_1_app(self):

        linear_test = {}

        length_scale = 32

        pw_vec_w = 0
        pr_vec_w = 1

        pr_raw_idx_vec_w = 0
        pw_raw_idx_vec_w = 1
        raw_comp_vec_w = LFComparisonOperator.LT.value
        raw_scalar_vec_w = 0
        raw_constraint_vec_w = (pr_vec_w, pr_raw_idx_vec_w,
                                pw_vec_w, pw_raw_idx_vec_w, raw_comp_vec_w, raw_scalar_vec_w)

        pr_war_idx_vec_w = 0
        pw_war_idx_vec_w = 1
        war_comp_vec_w = LFComparisonOperator.GT.value
        war_scalar_vec_w = 2
        war_constraint_vec_w = (pw_vec_w, pw_war_idx_vec_w, pr_vec_w,
                                pr_war_idx_vec_w, war_comp_vec_w, war_scalar_vec_w)

        in_size = 64 * 65
        out_size = 64 * 65

        linear_test[0] = {
            'type': Direction.IN,
            'name': 'port_w0',
            'config': {
                'dimensionality': 1,
                # 'extents': [16 * length_scale],
                'extents': [in_size],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 4
                }
            },
            'vec_in_config': {
                'dimensionality': 2,
                'extents': [4, 16 * length_scale],
                'address': {
                    'strides': [1, 4],
                    'offset': 0
                },
                'schedule': {
                    'strides': [1, 4],
                    'offset': 0
                }
            },
            'vec_out_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 4
                }
            },
            'vec_constraints': [raw_constraint_vec_w, war_constraint_vec_w]
        }

        pw_vec_r = 0
        pr_vec_r = 1

        pr_raw_idx_vec_r = 1
        pw_raw_idx_vec_r = 0
        raw_comp_vec_r = LFComparisonOperator.LT.value
        raw_scalar_vec_r = 0
        raw_constraint_vec_r = (pr_vec_r, pr_raw_idx_vec_r,
                                pw_vec_r, pw_raw_idx_vec_r, raw_comp_vec_r, raw_scalar_vec_r)

        pr_war_idx_vec_r = 1
        pw_war_idx_vec_r = 0
        war_comp_vec_r = LFComparisonOperator.GT.value
        war_scalar_vec_r = 2
        war_constraint_vec_r = (pw_vec_r, pw_war_idx_vec_r, pr_vec_r,
                                pr_war_idx_vec_r, war_comp_vec_r, war_scalar_vec_r)

        linear_test[2] = {
            'type': Direction.OUT,
            'name': 'port_r0',
            'config': {
                'dimensionality': 1,
                # 'extents': [16 * length_scale],
                'extents': [out_size],
                'address': {
                    'strides': [1],
                    # Start this here to handle the bogus data creation
                    'offset': -64
                },
                'schedule': {
                    'strides': [4],
                    'offset': 17
                }
            },
            'vec_in_config': {
                'dimensionality': 1,
                'extents': [16 * length_scale],
                'address': {
                    'strides': [1],
                    'offset': 0
                },
                'schedule': {
                    'strides': [4],
                    'offset': 18
                }
            },
            'vec_out_config': {
                'dimensionality': 2,
                'extents': [4, 16 * length_scale],
                'address': {
                    'strides': [1, 4],
                    'offset': 0
                },
                'schedule': {
                    'strides': [1, 4],
                    'offset': 19
                }
            },
            'vec_constraints': [raw_constraint_vec_r, war_constraint_vec_r]
        }

        pw = 0
        pr = 2

        pr_raw_idx = 0
        pw_raw_idx = 0
        raw_comp = LFComparisonOperator.LT.value
        # raw_scalar = 4
        # Allows the reads to start early
        raw_scalar = -64
        raw_constraint = (pr, pr_raw_idx, pw, pw_raw_idx, raw_comp, raw_scalar)

        pw_war_idx = 0
        pr_war_idx = 0
        war_comp = LFComparisonOperator.GT.value
        war_scalar = 100
        war_constraint = (pw, pw_war_idx, pr, pr_war_idx, war_comp, war_scalar)

        # Just have read follow write
        linear_test['constraints'] = [raw_constraint, war_constraint]

        return linear_test

    def convert_app_json_to_config(self, app_json):
        # Get each port...
        ret_config = {}
        app_port_mappings = app_json["port_mappings"]
        all_used_ports = []
        for orig_port, new_port in app_port_mappings.items():
            all_used_ports.append(new_port)

        print("ALL USED PORTS")
        print(all_used_ports)

        for used_port in all_used_ports:
            port_config = self.get_base_port_config(used_port)

            port_access_map = app_json["access_map"][used_port]
            port_id = app_json["domain"][used_port]

            port_config["config"]["dimensionality"] = port_access_map["dimensionality"][0]
            port_config["config"]["extents"] = port_id["extents"]
            port_config["config"]["address"] = {
                'strides': port_access_map["address_stride"],
                'offset': port_access_map["address_offset"][0]
            }
            port_config["config"]["schedule"] = {
                # 'strides': port_access_map["address_stride"],
                # 'offset': port_access_map["address_offset"][0]
            }

            port_config["vec_in_config"] = {}
            port_config["vec_out_config"] = {}
            port_config["vec_constraints"] = []

            ret_config[self.port_name_to_int(used_port)] = port_config

        # Process the dependencies...
        ret_config["constraints"] = []
        deps_map = app_json["dep_values"]
        # (pr, pr_raw_idx, pw, pw_raw_idx, raw_comp, raw_scalar)
        # LFComparisonOperator.LT.value
        for dep_pair, indices in deps_map.items():
            this_depends, on_this = dep_pair
            this_depends_int = self.port_name_to_int(this_depends)
            on_this_int = self.port_name_to_int(on_this)
            # This is RAW
            comparison = None
            if "port_r" in this_depends:
                comparison = LFComparisonOperator.LT.value
            elif "port_w" in this_depends:
                comparison = LFComparisonOperator.GT.value

            hack = False
            this_depends_idx = None
            on_this_idx = None
            scalar = None

            # This is WAR
            if indices is None:
                assert "port_w" in this_depends
                # Fake something for now...
                if hack:
                    this_depends_idx = 1
                    on_this_idx = 1
                    # TODO: This should be based on the size of the memory if there
                    # is no specific scalar set...
                    scalar = 8 # should NOT pass

                else:
                    this_depends_idx = 1
                    on_this_idx = 1
                    scalar = 8

            else:
                this_depends_idx = indices[0]
                on_this_idx = indices[1]
                scalar_adjust = 0
                # This should be computed based on the precursor deltas...
                # It should be provided in a richer way, but start with 0, then
                if this_depends_idx == 0 and on_this_idx == 0:
                    # scalar = 9
                    scalar = 0
                else:
                    scalar = 0
                # Check the precursor deltas and project them into the level of the dependency...
                if "precursor_deltas" not in app_json:
                    app_json["precursor_deltas"] = {}
                if this_depends not in app_json["precursor_deltas"]:
                    app_json["precursor_deltas"][this_depends] = [[0, 0]]
                prec_delts = app_json["precursor_deltas"][this_depends]
                print("Precursor deltas...")
                for pd in prec_delts:
                    pd_idx = pd[0]
                    pd_val = pd[1]
                    if pd_val == 0:
                        continue
                    # If the pd_val is nonzero - let's try overriding the dependency
                    # this_depends_idx = pd_idx
                    # on_this_idx = pd_idx
                    # scalar_adjust = pd_val
                    scalar_adjust = 0
                    scalar_adjust_addr = pd_val
                    # If they are the same, just copy the value, otherwise multiply by all extents in the difference
                    # down to the current level...
                    # for i in range(pd_idx):
                    #     scalar_adjust_addr *= app_json["domain"][this_depends]["extents"][i]
                    if pd_idx > this_depends_idx:
                        for i in range(pd_idx - this_depends_idx):
                            scalar_adjust_addr *= app_json["domain"][this_depends]["extents"][i]
                    # Only allow one level to have precursor...
                    # Also offset the starting address by the scalar_adjust...
                    ret_config[self.port_name_to_int(this_depends)]["config"]["address"]["offset"] -= scalar_adjust_addr

                    # Clamp scalar adjust to size of extent
                    if scalar_adjust_addr >= app_json["domain"][this_depends]["extents"][this_depends_idx]:
                        scalar_adjust = app_json["domain"][this_depends]["extents"][this_depends_idx] - 1
                    else:
                        scalar_adjust = scalar_adjust_addr
                    break

                scalar -= scalar_adjust

            ret_config["constraints"].append((this_depends_int, this_depends_idx,
                                              on_this_int, on_this_idx, comparison, scalar
                                              ))

        return ret_config

    def get_port_from_idx(self, port_num):
        assert port_num < (self.get_num_in_ports() + self.get_num_out_ports()), f"Only have {self.get_num_in_ports() + self.get_num_out_ports()} ports"
        port: Port = None
        if port_num < self.get_num_in_ports():
            port = self.get_in_ports()[port_num]
        else:
            port = self.get_out_ports()[port_num - self.get_num_in_ports()]
        return port

    def rewrite_app_json(self, app_json):
        app_json_copy = {}
        ret_map = {}
        # Copy the app_json
        for key, val in app_json.items():
            app_json_copy[key] = val

        assert "port_mappings" in app_json_copy
        port_mappings = app_json_copy["port_mappings"]
        port_mappings_keys = port_mappings.keys()

        # Go through port mappings and replace data_in_X, data_in_pond_X, data_out_X, data_out_pond_X
        for key, val in port_mappings.items():

            if "data_in_pond_" in val:
                get_int = int(port_mappings[key].split("data_in_pond_")[1])
                final_name = f"port_w{get_int}"
                port_mappings[key] = final_name
            if "data_out_pond_" in val:
                get_int = int(port_mappings[key].split("data_out_pond_")[1])
                final_name = f"port_r{get_int}"
                port_mappings[key] = final_name
            if "data_in_" in val and "data_in_pond_" not in val:
                get_int = int(port_mappings[key].split("data_in_")[1])
                final_name = f"port_w{get_int}"
                port_mappings[key] = final_name
            if "data_out_" in val and "data_out_pond_" not in val:
                get_int = int(port_mappings[key].split("data_out_")[1])
                final_name = f"port_r{get_int}"
                port_mappings[key] = final_name

        ret_map['port_mappings'] = port_mappings

        # Now go through and remap all mentions of each port name
        for key, val in app_json_copy.items():

            # Handle the dep_values separately
            if "dep_values" in key:
                # Split on ___DEPTO___
                copy_dict = {}
                for k2, v2 in val.items():
                    split_key = k2.split("___DEPTO___")
                    this_depends = port_mappings[split_key[0]]
                    on_this = port_mappings[split_key[1]]
                    copy_dict[(this_depends, on_this)] = v2
                ret_map[key] = copy_dict
                continue

            # Only one level deep...
            # Check if val is a dict
            key_use = key
            if key in port_mappings_keys:
                key_use = port_mappings[key]

            val_use = val
            if type(val) is dict:
                val_copy_dict = {}
                for k2, v2 in val.items():
                    key2_use = k2
                    if k2 in port_mappings_keys:
                        key2_use = port_mappings[k2]
                    val_copy_dict[key2_use] = v2
                val_use = val_copy_dict
            elif type(val) is str:
                if val in port_mappings_keys:
                    val_use = port_mappings[val]

            ret_map[key_use] = val_use

        return ret_map

    def gen_bitstream(self, application):
        '''Overall flow of the bitstreams is to basically go through each port and map down the information.
           There may be other information that needs to go into the configuration, but that could be in the object hierarchy
        '''

        test_name = os.environ.get("TEST_NAME_FOR_HACKING_CHECK", None)
        override = test_name in APPS_NEEDING_HACKS
        if override is True:
            application = hack_rv_config(test_name)
            print("HARDCODED APPLICATION")
            print(application)
        else:
            print("Producing SPEC BITSTREAM with Application:")
            print("APPLICATION BEFORE")
            print(application)

            application = self.rewrite_app_json(application)
            print("APPLICATION AFTER")
            print(application)

            application = self.convert_app_json_to_config(application)
            print("APPLICATION AFTER _config")
            print(application)

        # Need to integrate all the bitstream information
        # into one single integer/string for the verilog harness
        self.clear_configuration()

        # Each piece in the application is a port
        for port_num, maps in application.items():

            if type(port_num) is not int:
                continue

            # Get the port and associated controllers
            port: Port = self.get_port_from_idx(port_num)

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
                vec_constraints = None
                if self.any_rv_sg:
                    vec_constraints = maps['vec_constraints']
                port_bs = port.gen_bitstream(vec_in=maps['vec_in_config'],
                                             vec_out=maps['vec_out_config'],
                                             vec_constraints=vec_constraints)
                self.configure(port, port_bs)

            # All components should be aware of RV/Static context
            id_bs = port_id.gen_bitstream(port_config['dimensionality'], port_config['extents'], self.any_rv_sg)
            ag_bs = port_ag.gen_bitstream(addr_map, extents=port_config['extents'],
                                          dimensionality=port_config['dimensionality'])
            if self.any_rv_sg:
                sg_bs = port_sg.gen_bitstream(sched_map, extents=port_config['extents'],
                                              dimensionality=port_config['dimensionality'])
                # Now also configure all rv comparison network
                # comparisons = []
                # self.rv_comparison_network.gen_bitstream()
            else:
                # sg_bs = port_sg.gen_bitstream(sched_map)
                sg_bs = port_sg.gen_bitstream(sched_map, extents=port_config['extents'],
                                              dimensionality=port_config['dimensionality'])

            # Create a clear configuration
            self.configure(port_id, id_bs)
            self.configure(port_ag, ag_bs)
            self.configure(port_sg, sg_bs)

        # Need to program the comparison network as well in ready/valid TODO: separate all the configs into individual SG
        if self.any_rv_sg:
            # get application constraints and pass them to rv_comparison_network
            constraints = application['constraints']
            rv_comp_bs = self.rv_comparison_network.gen_bitstream(constraints=constraints)
            self.configure(self.rv_comparison_network, rv_comp_bs)

        self.create_config_int()
        return self.get_config_int()
