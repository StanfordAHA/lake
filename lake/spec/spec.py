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
from lake.utils.util import connect_memoryport_storage, inline_multiplexer, shift_reg, round_up_to_power_of_2
from lake.modules.rv_comparison_network import RVComparisonNetwork
from lake.spec.component import Component
from lake.modules.ready_valid_interface import RVInterface
from lake.modules.arbiter import Arbiter
from lake.spec.reg_fifo import RegFIFO
import math
import json


class Spec():
    """Harness for lake primitives
    """
    def __init__(self, name="lakespec", clkgate=True,
                 config_passthru=True,
                 opt_rv=False) -> None:
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
        self.num_mem_ports = 0
        self.fw_max = 1
        self.clk_gate = clkgate
        self.config_passthru = config_passthru
        self.opt_rv = opt_rv

    def register_(self, comp):
        self._hw_graph.add_node(comp)
        self._hw_graph[comp]['index'] = self._num_nodes
        self._index_to_node[self._num_nodes] = comp
        self._num_nodes += 1

    def get_num_ports(self):
        return self.num_ports

    def get_node_from_idx(self, idx, verbose=False):
        if verbose:
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
                elif pdir == Direction.OUT:
                    self.num_out_ports += 1
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

        self._final_gen = Component(name=self._name)
        # Before we go into the port loop, if any of the schedule generators is dynamic (vs. static), we need to know this and collect
        # information before genning the hardware

        # Just instantiate one comparison thing for now...
        if self.any_rv_sg:
            # In later stage of spec hw gen will add each read and write port to this...
            self.rv_comparison_network = RVComparisonNetwork(name='rv_comparison_network_this_spec')

        self.hw_attr = {}
        self.hw_attr['clk'] = self._final_gen.get_clock()
        self.hw_attr['rst_n'] = self._final_gen.get_reset()

        # First generate the storages based on the ports connected to them and their capacities
        storage_nodes = self.get_nodes(Storage)

        for j_, storage_node in enumerate(storage_nodes):
            storage_node: Storage
            # get MemoryPorts
            memoryports = list(nx.neighbors(self._hw_graph, storage_node))
            storage_node.gen_hardware(pos_reset=False, memory_ports=memoryports)
            # memoryports = nx.neighbors(self._hw_graph, storage_node)
            # Now we have the storage generated, want to generate the memoryports hardware which will be simply
            # passthru of the port currently...
            # Now generate a storage element based on all of these ports and add them to the final generator
            self._final_gen.add_child("storage", storage_node, clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'])

            strg_intfs = storage_node.get_memport_intfs()

            # Build memory ports
            memoryports = nx.neighbors(self._hw_graph, storage_node)
            for i_, mp in enumerate(memoryports):
                mp: MemoryPort
                mp.gen_hardware(pos_reset=False, storage_node=storage_node)
                self._final_gen.add_child(f"memoryport_{i_}_storage_{j_}", mp,
                                          clk=self.hw_attr['clk'],
                                          rst_n=self.hw_attr['rst_n'])
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
            self._final_gen.wire(port_id.ports.restart, port_ag.ports.restart)
            self._final_gen.wire(port_id.ports.iterators, port_ag.ports.iterators)

            self._final_gen.wire(port_id.ports.mux_sel, port_sg.ports.mux_sel)
            self._final_gen.wire(port_id.ports.restart, port_sg.ports.restart)
            self._final_gen.wire(port_id.ports.iterators, port_sg.ports.iterators)

            # Send through the extents to sg if there is RV
            if self.any_rv_sg:
                self._final_gen.wire(port_id.ports.extents_out, port_sg.ports.extents)

            # Gen the hardware for each, assemble the signals
            assembled_port = {}
            assembled_port['dir'] = port.get_direction()
            assembled_port['data'] = port.get_mp_intf()['data']
            assembled_port['addr'] = port_ag.get_address()
            # assembled_port['en'] = quali_step
            # assembled_port['en'] = port_sg.get_step()

            # send signals through memintf decoder (one port to many memoryports, doing decoding based on address)
            memintf_dec = MemoryInterfaceDecoder(name=f"memintfdec_{i_}", port_type=port.get_direction(),
                                                 port_intf=assembled_port, memports=memports_,
                                                 runtime=self.runtime)
            memintf_dec.gen_hardware()

            self._final_gen.add_child(f"memintfdec_inst_{i_}", memintf_dec,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'])
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
                    self._final_gen.wire(port_ag.ports.step, mid_grant)
                    self._final_gen.wire(port_id.ports.step, mid_grant)
                    # Wire the ID step
                # If it is an OUT Port, we need to qualify the step with the grant line from the arbitration and the
                # downstream ready (from the Port I guess)
                elif port_direction == Direction.OUT:
                    quali_step = port_sg.ports.step
                    # quali_step = self.qualify_step(port_sg, Direction.OUT)

                    port_ready = port.get_mp_intf()['ready']
                    sg_step = port_sg.ports.step
                    mid_grant = memintf_dec.get_p_intf()['grant']
                    # The enable to mid is memintf decoder resource available ready + step
                    quali_step = sg_step & memintf_dec.ports.resource_ready
                    # quali_step = sg_step & port_ready
                    # The grant is the ready/final step to ID, AG, ready
                    # self._final_gen.wire(port.get_mp_intf()['valid'], mid_grant)
                    self._final_gen.wire(port.get_mp_intf()['valid'], memintf_dec.ports.data_valid)
                    # The ready comes out of the memintf decoder
                    self._final_gen.wire(memintf_dec.ports.data_ready, port_ready)

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
                if self.opt_rv and port_direction == Direction.OUT:

                    # For this port, we want to give it the step from the SG and the addr from the AG
                    self._final_gen.wire(quali_step, port.ports.sg_step_in)
                    self._final_gen.wire(port_ag.get_address(), port.ports.addr_in)

                else:
                    ext_intf = port.get_internal_ag_intf()

                    if port_direction == Direction.IN:
                        # For a write Port, just directly connect everything
                        self._final_gen.wire(ext_intf['step'], quali_step)
                        self._final_gen.wire(ext_intf['mux_sel'], port_id.ports.mux_sel)
                        self._final_gen.wire(ext_intf['iterators'], port_id.ports.iterators)
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
            ub_intf = port.get_ub_intf()
            if port.get_direction() == Direction.IN:
                if self.any_rv_sg:
                    p_temp_rv = self._final_gen.rvinput(name=f"port_{i_}", width=ub_intf['data'].width)
                    p_temp = p_temp_rv.get_port()
                    p_temp_valid = p_temp_rv.get_valid()
                    p_temp_ready = p_temp_rv.get_ready()
                    self._final_gen.wire(p_temp_valid, ub_intf['valid'])
                    self._final_gen.wire(p_temp_ready, ub_intf['ready'])
                else:
                    p_temp = self._final_gen.input(f"port_{i_}", width=ub_intf['data'].width)
                    p_temp_valid = self._final_gen.input(f"port_{i_}_valid", 1)
                    p_temp_ready = self._final_gen.output(f"port_{i_}_ready", 1)
                    # self._final_gen.wire(p_temp_ready, kts.const(1, 1))
                    self._final_gen.wire(p_temp_ready, ub_intf['ready'])
                    self._final_gen.wire(ub_intf['valid'], p_temp_valid)
            elif port.get_direction() == Direction.OUT:
                if self.any_rv_sg:
                    p_temp_rv = self._final_gen.rvoutput(name=f"port_{i_}", width=ub_intf['data'].width)
                    p_temp = p_temp_rv.get_port()
                    p_temp_valid = p_temp_rv.get_valid()
                    p_temp_ready = p_temp_rv.get_ready()
                    self._final_gen.wire(p_temp_valid, ub_intf['valid'])
                    self._final_gen.wire(p_temp_ready, ub_intf['ready'])
                else:
                    p_temp = self._final_gen.output(f"port_{i_}", width=ub_intf['data'].width)
                    p_temp_valid = self._final_gen.output(f"port_{i_}_valid", 1)
                    p_temp_ready = self._final_gen.input(f"port_{i_}_ready", 1)
                    # self._final_gen.wire(p_temp_valid, kts.const(1, 1))
                    self._final_gen.wire(p_temp_valid, ub_intf['valid'])
                    self._final_gen.wire(ub_intf['ready'], p_temp_ready)

            self._final_gen.wire(p_temp, ub_intf['data'])

        # Now can build all the muxing in between the memintf and the mps
        self.build_mp_p()

        # Can now build the comparison network if it's there...
        if self.any_rv_sg:
            self.rv_comparison_network.gen_hardware()
            self._final_gen.add_child('rv_comp_network_top_spec', self.rv_comparison_network,
                                      clk=self.hw_attr['clk'],
                                      rst_n=self.hw_attr['rst_n'])
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

    def insert_arbiter(self, reqs, grants, arb_name):
        '''This function adds an arbiter, just pass in the req lines and it will produce an object
            with both a reqs and grants field
        '''
        tmp_arb = Arbiter(ins=len(reqs))
        self._final_gen.add_child(arb_name, tmp_arb,
                                  clk=self.hw_attr['clk'],
                                  rst_n=self.hw_attr['rst_n'],
                                  clk_en=kts.const(1, 1),
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

    def configure(self, node, bs, verbose=False):
        # node_config_base = self.get_config_base(node)
        if verbose:
            print("Showing all child bases...")
            print(self._final_gen.child_cfg_bases)
        node_config_base = self._final_gen.child_cfg_bases[node]
        for reg_bound, value in bs:
            upper, lower = reg_bound
            if verbose:
                print(f"{upper},{lower} = {value}")
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

    def gen_bitstream(self, application):
        '''Overall flow of the bitstreams is to basically go through each port and map down the information.
           There may be other information that needs to go into the configuration, but that could be in the object hierarchy
        '''

        # Need to integrate all the bitstream information
        # into one single integer/string for the verilog harness
        self.clear_configuration()

        # Each piece in the application is a port
        for port_num, maps in application.items():

            if type(port_num) is not int:
                continue

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
