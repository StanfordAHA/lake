import math
from lake.top.memory_controller import MemoryController
from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator, ReadyValidScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.top.tech_maps import GF_Tech_Map


def build_four_port_wide_fetch_rv(storage_capacity=16384, data_width=16, dims: int = 6, vec_width=4, physical=True,
                                  reg_file=False, vec_capacity=2, opt_rv=True, remote_storage=True, id_width=16, add_filter_path=True) -> Spec:

    # TODO: Override this in garnet and not here...
    id_width = 11

    ls = Spec(name="lakespec_mem", opt_rv=opt_rv, remote_storage=remote_storage, run_flush_pass=False,
              config_passthru=True, comply_17=True)

    # Don't opt timing on the in ports (which really just adds a fifo at the input which we don't need)
    in_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                   vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.IN,
                   opt_rv=opt_rv, opt_timing=False,
                   filter=True)
    in_port2 = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                    vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.IN,
                    opt_rv=opt_rv, opt_timing=False,
                    filter=True)
    out_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                    vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.OUT,
                    opt_rv=opt_rv)
    out_port2 = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                     vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.OUT,
                     opt_rv=opt_rv)

    ls.register(in_port, in_port2)

    if add_filter_path:
        in_port_filter = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC,
                              direction=Direction.IN, opt_rv=opt_rv, filter=True)
        ls.register(in_port_filter)

    ls.register(out_port, out_port2)

    if add_filter_path:

        out_port_filter = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC,
                              direction=Direction.OUT, opt_rv=opt_rv, filter=False)
        ls.register(out_port_filter)

    in_id = IterationDomain(dimensionality=dims, extent_width=id_width)
    in_ag = AddressGenerator(dimensionality=dims)
    in_sg = ReadyValidScheduleGenerator(dimensionality=dims)

    in_id2 = IterationDomain(dimensionality=dims, extent_width=id_width)
    in_ag2 = AddressGenerator(dimensionality=dims)
    in_sg2 = ReadyValidScheduleGenerator(dimensionality=dims)

    out_id = IterationDomain(dimensionality=dims, extent_width=id_width)
    out_ag = AddressGenerator(dimensionality=dims)
    out_sg = ReadyValidScheduleGenerator(dimensionality=dims)

    out_id2 = IterationDomain(dimensionality=dims, extent_width=id_width)
    out_ag2 = AddressGenerator(dimensionality=dims)
    out_sg2 = ReadyValidScheduleGenerator(dimensionality=dims)

    ls.register(in_id, in_ag, in_sg)
    ls.register(in_id2, in_ag2, in_sg2)
    ls.register(out_id, out_ag, out_sg)
    ls.register(out_id2, out_ag2, out_sg2)

    if add_filter_path:
        in_id_filter = IterationDomain(dimensionality=dims, extent_width=id_width)
        in_ag_filter = AddressGenerator(dimensionality=dims)
        in_sg_filter = ReadyValidScheduleGenerator(dimensionality=dims)

        out_id_filter = IterationDomain(dimensionality=dims, extent_width=id_width)
        out_ag_filter = AddressGenerator(dimensionality=dims)
        out_sg_filter = ReadyValidScheduleGenerator(dimensionality=dims)

        ls.register(in_id_filter, in_ag_filter, in_sg_filter)
        ls.register(out_id_filter, out_ag_filter, out_sg_filter)

    data_bytes = (data_width * vec_width) // 8
    tech_map = None
    if physical:
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes, width=data_width * vec_width, dual_port=False)

    # 1024 Bytes
    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map, remote=True)
    shared_rw_mem_port = MemoryPort(data_width=data_width * vec_width, mptype=MemoryPortType.RW, delay=1)
    ls.register(stg, shared_rw_mem_port)

    if add_filter_path:
        # Just try buffering 8 data for now ... want to turn into a fifo if possible.
        filter_cap = data_bytes * 8
        stg_filter = SingleBankStorage(capacity=filter_cap, remote=False)
        write_port_filter = MemoryPort(data_width=data_width, mptype=MemoryPortType.W, delay=1)
        read_port_filter = MemoryPort(data_width=data_width, mptype=MemoryPortType.R, delay=1)
        ls.register(stg_filter, write_port_filter, read_port_filter)

    # All cores are registered at this point
    # Now connect them

    # In to in
    ls.connect(in_port, in_id)
    ls.connect(in_port, in_ag)
    ls.connect(in_port, in_sg)

    ls.connect(in_port2, in_id2)
    ls.connect(in_port2, in_ag2)
    ls.connect(in_port2, in_sg2)

    # Out to out
    ls.connect(out_port, out_id)
    ls.connect(out_port, out_ag)
    ls.connect(out_port, out_sg)

    ls.connect(out_port2, out_id2)
    ls.connect(out_port2, out_ag2)
    ls.connect(out_port2, out_sg2)

    # In and Out to shared memory port
    ls.connect(in_port, shared_rw_mem_port)
    ls.connect(in_port2, shared_rw_mem_port)
    ls.connect(out_port, shared_rw_mem_port)
    ls.connect(out_port2, shared_rw_mem_port)

    # Memory Ports to storage
    ls.connect(shared_rw_mem_port, stg)

    if add_filter_path:
        # In to filter
        ls.connect(in_port_filter, in_id_filter)
        ls.connect(in_port_filter, in_ag_filter)
        ls.connect(in_port_filter, in_sg_filter)

        # Out to filter
        ls.connect(out_port_filter, out_id_filter)
        ls.connect(out_port_filter, out_ag_filter)
        ls.connect(out_port_filter, out_sg_filter)

        # In and Out to filter memory ports
        ls.connect(in_port_filter, write_port_filter)
        ls.connect(out_port_filter, read_port_filter)

        # Memory Ports to storage
        ls.connect(write_port_filter, stg_filter)
        ls.connect(read_port_filter, stg_filter)

    return ls


def build_pond_rv(storage_capacity: int = 64, data_width=16,
                  dims: int = 6, physical=False, reg_file=True,
                  remote_storage=True, opt_rv=True) -> Spec:

    # TODO: Override this in garnet and not here...
    id_width = 11

    read_delay = 0

    ls = Spec(name="lakespec_pond", opt_rv=opt_rv, remote_storage=remote_storage, run_flush_pass=False,
              config_passthru=True, comply_17=True)

    in_port = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC,
                   direction=Direction.IN, opt_rv=opt_rv)
    # in_port2 = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC,
    #                 direction=Direction.IN, opt_rv=opt_rv)
    out_port = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC,
                    direction=Direction.OUT, opt_rv=opt_rv)
    out_port2 = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC,
                     direction=Direction.OUT, opt_rv=opt_rv)

    flush_port = Port(ext_data_width=data_width, runtime=Runtime.DYNAMIC,
                      direction=Direction.IN, opt_rv=opt_rv, dangling=True)

    # ls.register(in_port, in_port2, out_port, out_port2)
    ls.register(in_port, out_port, out_port2)
    ls.register(flush_port)
    # ls.register(in_port, out_port, out_port2)

    in_id = IterationDomain(dimensionality=dims, extent_width=id_width)
    in_ag = AddressGenerator(dimensionality=dims)
    in_sg = ReadyValidScheduleGenerator(dimensionality=dims)

    # in_id2 = IterationDomain(dimensionality=dims, extent_width=id_width)
    # in_ag2 = AddressGenerator(dimensionality=dims)
    # in_sg2 = ReadyValidScheduleGenerator(dimensionality=dims)

    out_id = IterationDomain(dimensionality=dims, extent_width=id_width)
    out_ag = AddressGenerator(dimensionality=dims)
    out_sg = ReadyValidScheduleGenerator(dimensionality=dims)

    out_id2 = IterationDomain(dimensionality=dims, extent_width=id_width)
    out_ag2 = AddressGenerator(dimensionality=dims)
    out_sg2 = ReadyValidScheduleGenerator(dimensionality=dims)

    flush_id = IterationDomain(dimensionality=dims, extent_width=id_width)
    flush_ag = AddressGenerator(dimensionality=dims)
    # Crucial to make sure the flushing is tied to the same schedule as the rest of the system
    flush_sg = ReadyValidScheduleGenerator(dimensionality=dims)

    ls.register(in_id, in_ag, in_sg)
    # ls.register(in_id2, in_ag2, in_sg2)
    ls.register(out_id, out_ag, out_sg)
    ls.register(out_id2, out_ag2, out_sg2)

    ls.register(flush_id, flush_ag, flush_sg)

    data_bytes = data_width // 8
    tech_map = None
    if physical:
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes, width=data_width, dual_port=True,
                               reg_file=reg_file)

    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map, remote=True)

    wr_mem_port = MemoryPort(data_width=16, mptype=MemoryPortType.W, delay=1)
    # wr_mem_port2 = MemoryPort(data_width=16, mptype=MemoryPortType.W, delay=1)
    # rd_wr_mem_port = MemoryPort(data_width=16, mptype=MemoryPortType.RW, delay=1)
    rd_mem_port = MemoryPort(data_width=16, mptype=MemoryPortType.R, delay=read_delay)
    rd_mem_port2 = MemoryPort(data_width=16, mptype=MemoryPortType.R, delay=read_delay)

    wr_mem_port_flush = MemoryPort(data_width=16, mptype=MemoryPortType.W, delay=read_delay, flush_mem=True)

    # rd_mem_port = MemoryPort(data_width=16, mptype=MemoryPortType.R, delay=read_delay)
    # rd_mem_port2 = MemoryPort(data_width=16, mptype=MemoryPortType.R, delay=read_delay)

    # ls.register(stg, wr_mem_port, wr_mem_port2, rd_mem_port, rd_mem_port2, stg)
    # ls.register(stg, rd_wr_mem_port, rd_mem_port)
    ls.register(stg, wr_mem_port, rd_mem_port, rd_mem_port2)
    # ls.register(stg, wr_mem_port, rd_mem_port, rd_mem_port2, stg)
    ls.register(wr_mem_port_flush)
    # All cores are registered at this point
    # Now connect them

    # In to in
    ls.connect(in_port, in_id)
    ls.connect(in_port, in_ag)
    ls.connect(in_port, in_sg)

    # ls.connect(in_port2, in_id2)
    # ls.connect(in_port2, in_ag2)
    # ls.connect(in_port2, in_sg2)

    # Out to out
    ls.connect(out_port, out_id)
    ls.connect(out_port, out_ag)
    ls.connect(out_port, out_sg)

    ls.connect(out_port2, out_id2)
    ls.connect(out_port2, out_ag2)
    ls.connect(out_port2, out_sg2)

    ls.connect(flush_port, flush_id)
    ls.connect(flush_port, flush_ag)
    ls.connect(flush_port, flush_sg)

    # In and Out to memory ports
    ls.connect(in_port, wr_mem_port)
    # ls.connect(in_port, rd_wr_mem_port)
    # ls.connect(in_port2, wr_mem_port2)
    # ls.connect(out_port, rd_wr_mem_port)
    ls.connect(out_port, rd_mem_port)
    ls.connect(out_port2, rd_mem_port2)

    ls.connect(flush_port, wr_mem_port_flush)

    # Memory Ports to storage
    ls.connect(wr_mem_port, stg)
    # ls.connect(rd_wr_mem_port, stg)
    ls.connect(rd_mem_port, stg)
    ls.connect(rd_mem_port2, stg)
    # ls.connect(wr_mem_port, stg)
    # ls.connect(wr_mem_port2, stg)
    # ls.connect(rd_mem_port, stg)
    # ls.connect(rd_mem_port2, stg)
    ls.connect(wr_mem_port_flush, stg)

    return ls


class SpecMemoryController(MemoryController):
    # Fix to quad port spec for now...
    def __init__(self, spec: Spec,
                 name="SpecMemoryController_default_name"):
        # super().__init__(name, debug, is_clone, internal_generator, exclusive, add_flush)
        self.spec = spec
        self.spec.set_name(name)

        print("Before hardware gen...")
        self.spec.generate_hardware()
        print("After hardware gen...")
        self.memory_ports = self.spec.get_memory_ports_mc()
        print(self.memory_ports)

        # Annotate liftable ports...
        self.spec.annotate_liftable_ports()
        # self.internal_generator = self.spec.get_generator())

        print("Before internal generator")
        print(self.spec.get_generator())
        print(self.spec.get_generator().child_generator())

        print("In spec control")
        # print(self.child_generator())
        # print(self.name)

        super().__init__(name=name, debug=True,
                         exclusive=False, add_flush=True, internal_generator=self.spec.get_internal_generator(), is_clone=False)

        print("After internal generator")
        print(self.child_generator())

        # # Now copy child generators? Everything else is the same????
        self.child_generator().update(self.spec.get_generator().child_generator())
        print(self.child_generator())

    def get_config_mode_str(self):
        return "lakespec"

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return self.memory_ports

    def get_verilog(self, output_dir="."):
        self.spec.get_verilog(output_dir=output_dir)

    def print_name(self):
        print(self.spec._name)

    def get_bitstream(self, config_json, prefix="", **kwargs):
        print("in spec config bitstream...")
        print(config_json)
        bs = self.spec.gen_bitstream(config_json)
        bs_full = [('config_memory', bs)]
        return bs_full


if __name__ == "__main__":

    # get the spec
    spec = build_pond_rv()

    spec.generate_hardware()
    spec.get_verilog("/aha/")
    # # Instantiate the core
    # smc = SpecMemoryController(spec=spec)
    # mp = smc.get_memory_ports()
    # print(f"Memory Ports: {mp}")
    # smc.get_verilog()
    # smc.print_name()
    # print("Done!")

# ---------------------------------------------------------------------------
# Parameterized spec builders.
#
# These live here (rather than in a sweep driver) because they are pure lake:
# they construct a Spec from lake primitives and nothing else. garnet's
# cgra/util_onyx.py imports them to build a memory core from a spec config,
# so they must be importable anywhere lake is -- including inside the rtl
# docker container, which ships lake but not the aha driver package.
# ---------------------------------------------------------------------------


def build_spec(storage_capacity=4096, data_width=16, vec_width=4,
               dims=6, in_ports=2, out_ports=2, dual_port=False,
               vec_capacity=2, max_extent=None, max_sequence_width=None,
               physical=False):
    """Factory function to build a lake Spec for any thesis configuration.

    Args:
        storage_capacity: SRAM capacity in bytes.
        data_width: External data width in bits.
        vec_width: Vector / fetch width (1 = no vectorization).
        dims: Number of iteration domain dimensions.
        in_ports: Number of input ports.
        out_ports: Number of output ports.
        dual_port: Whether to use separate R/W MemoryPorts (True) or shared RW (False).
        vec_capacity: Vector capacity for SIPO/PISO buffers.
        max_extent: Maximum iteration extent (affects counter upper bound).
        max_sequence_width: Maximum sequence width (affects stride width).
        physical: Use a physical SRAM tech map.

    Returns:
        A lake Spec instance.
    """
    id_width = 11
    if max_extent is not None:
        id_width = max(1, math.ceil(math.log2(max(max_extent, 2))))
    stride_width = 16
    if max_sequence_width is not None:
        stride_width = max(1, math.ceil(math.log2(max(max_sequence_width, 2))))

    ls = Spec(name="lakespec", opt_rv=False, remote_storage=True,
              config_passthru=False, comply_17=True)

    vc = vec_capacity if vec_width > 1 else None
    int_dw = data_width * vec_width

    # Create input ports
    input_port_list = []
    for _ in range(in_ports):
        p = Port(ext_data_width=data_width, int_data_width=int_dw,
                 vec_capacity=vc, runtime=Runtime.STATIC, direction=Direction.IN,
                 opt_rv=False, opt_timing=False)
        input_port_list.append(p)
    ls.register(*input_port_list)

    # Create output ports
    output_port_list = []
    for _ in range(out_ports):
        p = Port(ext_data_width=data_width, int_data_width=int_dw,
                 vec_capacity=vc, runtime=Runtime.STATIC, direction=Direction.OUT)
        output_port_list.append(p)
    ls.register(*output_port_list)

    # Create controllers for each port
    in_controllers = []
    for _ in range(in_ports):
        id_ = IterationDomain(dimensionality=dims, extent_width=id_width)
        ag_ = AddressGenerator(dimensionality=dims)
        sg_ = ScheduleGenerator(dimensionality=dims, stride_width=stride_width)
        ls.register(id_, ag_, sg_)
        in_controllers.append((id_, ag_, sg_))

    out_controllers = []
    for _ in range(out_ports):
        id_ = IterationDomain(dimensionality=dims, extent_width=id_width)
        ag_ = AddressGenerator(dimensionality=dims)
        sg_ = ScheduleGenerator(dimensionality=dims, stride_width=stride_width)
        ls.register(id_, ag_, sg_)
        out_controllers.append((id_, ag_, sg_))

    # Create storage and memory ports
    tech_map = None
    if physical:
        from lake.top.tech_maps import GF_Tech_Map
        data_bytes = (data_width * vec_width) // 8
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes,
                               width=data_width * vec_width,
                               dual_port=dual_port)
    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map, remote=True)
    if dual_port:
        write_mp = MemoryPort(data_width=data_width * vec_width,
                              mptype=MemoryPortType.W, delay=1)
        read_mp = MemoryPort(data_width=data_width * vec_width,
                             mptype=MemoryPortType.R, delay=1)
        ls.register(stg, write_mp, read_mp)
    else:
        shared_mp = MemoryPort(data_width=data_width * vec_width,
                               mptype=MemoryPortType.RW, delay=1)
        ls.register(stg, shared_mp)

    # Connect ports to controllers
    for i in range(in_ports):
        id_, ag_, sg_ = in_controllers[i]
        ls.connect(input_port_list[i], id_)
        ls.connect(input_port_list[i], ag_)
        ls.connect(input_port_list[i], sg_)

    for i in range(out_ports):
        id_, ag_, sg_ = out_controllers[i]
        ls.connect(output_port_list[i], id_)
        ls.connect(output_port_list[i], ag_)
        ls.connect(output_port_list[i], sg_)

    # Connect ports to memory ports
    if dual_port:
        for p in input_port_list:
            ls.connect(p, write_mp)
        for p in output_port_list:
            ls.connect(p, read_mp)
        ls.connect(write_mp, stg)
        ls.connect(read_mp, stg)
    else:
        for p in input_port_list:
            ls.connect(p, shared_mp)
        for p in output_port_list:
            ls.connect(p, shared_mp)
        ls.connect(shared_mp, stg)

    return ls


def build_spec_rv(storage_capacity=4096, data_width=16, vec_width=4,
                  dims=6, in_ports=2, out_ports=2, dual_port=False,
                  vec_capacity=2, max_extent=None, max_sequence_width=None,
                  add_filter_path=True, physical=True):
    """Factory function to build an RV (ready-valid / dynamic) lake Spec.

    Mirrors build_four_port_wide_fetch_rv but accepts configurable params
    matching the thesis experiment sweep space.  The resulting Spec uses
    Runtime.DYNAMIC, opt_rv=True, ReadyValidScheduleGenerator, and
    remote_storage=True — suitable for wrapping in SpecMemoryController
    to generate garnet hardware.

    Args:
        storage_capacity: SRAM capacity in bytes.
        data_width: External data width in bits.
        vec_width: Vector / fetch width (1 = no vectorization).
        dims: Number of iteration domain dimensions.
        in_ports: Number of input data ports.
        out_ports: Number of output data ports.
        dual_port: Whether to use separate R/W MemoryPorts or shared RW.
        vec_capacity: Vector capacity for SIPO/PISO buffers.
        max_extent: Maximum iteration extent (affects counter upper bound).
        max_sequence_width: Maximum sequence width (affects stride width).
        add_filter_path: Add a filter input/output path (matches default garnet).
        physical: Use a physical SRAM tech map.

    Returns:
        A lake Spec instance configured for RV hardware generation.
    """
    if vec_width > 1 and vec_capacity > 2:
        raise ValueError(
            "RV wide-fetch currently supports vec_capacity <= 2 only; "
            "larger SIPO/PISO cases are intentionally deferred"
        )

    id_width = 11
    if max_extent is not None:
        id_width = max(1, math.ceil(math.log2(max(max_extent, 2))))
    stride_width = 16
    if max_sequence_width is not None:
        stride_width = max(1, math.ceil(math.log2(max(max_sequence_width, 2))))

    ls = Spec(name="lakespec_mem", opt_rv=True, remote_storage=True,
              run_flush_pass=False, config_passthru=True, comply_17=True)

    vc = vec_capacity if vec_width > 1 else None
    int_dw = data_width * vec_width

    # Create input data ports
    input_port_list = []
    for _ in range(in_ports):
        p = Port(ext_data_width=data_width, int_data_width=int_dw,
                 vec_capacity=vc, runtime=Runtime.DYNAMIC, direction=Direction.IN,
                 opt_rv=True, opt_timing=False, filter=True)
        input_port_list.append(p)
    ls.register(*input_port_list)

    # Create filter input port
    if add_filter_path:
        in_port_filter = Port(ext_data_width=data_width,
                              int_data_width=data_width,
                              runtime=Runtime.DYNAMIC,
                              direction=Direction.IN, opt_rv=True, filter=True)
        ls.register(in_port_filter)

    # Create output data ports
    output_port_list = []
    for _ in range(out_ports):
        p = Port(ext_data_width=data_width, int_data_width=int_dw,
                 vec_capacity=vc, runtime=Runtime.DYNAMIC, direction=Direction.OUT,
                 opt_rv=True)
        output_port_list.append(p)
    ls.register(*output_port_list)

    # Create filter output port
    if add_filter_path:
        out_port_filter = Port(ext_data_width=data_width,
                               int_data_width=data_width,
                               runtime=Runtime.DYNAMIC,
                               direction=Direction.OUT, opt_rv=True, filter=False)
        ls.register(out_port_filter)

    # Create controllers for each data port
    in_controllers = []
    for _ in range(in_ports):
        id_ = IterationDomain(dimensionality=dims, extent_width=id_width)
        ag_ = AddressGenerator(dimensionality=dims)
        sg_ = ReadyValidScheduleGenerator(dimensionality=dims)
        ls.register(id_, ag_, sg_)
        in_controllers.append((id_, ag_, sg_))

    out_controllers = []
    for _ in range(out_ports):
        id_ = IterationDomain(dimensionality=dims, extent_width=id_width)
        ag_ = AddressGenerator(dimensionality=dims)
        sg_ = ReadyValidScheduleGenerator(dimensionality=dims)
        ls.register(id_, ag_, sg_)
        out_controllers.append((id_, ag_, sg_))

    # Create filter controllers
    if add_filter_path:
        in_id_filter = IterationDomain(dimensionality=dims, extent_width=id_width)
        in_ag_filter = AddressGenerator(dimensionality=dims)
        in_sg_filter = ReadyValidScheduleGenerator(dimensionality=dims)
        ls.register(in_id_filter, in_ag_filter, in_sg_filter)

        out_id_filter = IterationDomain(dimensionality=dims, extent_width=id_width)
        out_ag_filter = AddressGenerator(dimensionality=dims)
        out_sg_filter = ReadyValidScheduleGenerator(dimensionality=dims)
        ls.register(out_id_filter, out_ag_filter, out_sg_filter)

    # Create storage and memory ports
    from lake.top.tech_maps import GF_Tech_Map
    data_bytes = (data_width * vec_width) // 8
    tech_map = None
    if physical:
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes,
                               width=data_width * vec_width,
                               dual_port=dual_port)

    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map, remote=True)
    if dual_port:
        write_mp = MemoryPort(data_width=data_width * vec_width,
                              mptype=MemoryPortType.W, delay=1)
        read_mp = MemoryPort(data_width=data_width * vec_width,
                             mptype=MemoryPortType.R, delay=1)
        ls.register(stg, write_mp, read_mp)
    else:
        shared_mp = MemoryPort(data_width=data_width * vec_width,
                               mptype=MemoryPortType.RW, delay=1)
        ls.register(stg, shared_mp)

    # Create filter storage and memory ports
    if add_filter_path:
        filter_cap = data_bytes * 8
        stg_filter = SingleBankStorage(capacity=filter_cap, remote=False)
        write_port_filter = MemoryPort(data_width=data_width,
                                       mptype=MemoryPortType.W, delay=1)
        read_port_filter = MemoryPort(data_width=data_width,
                                      mptype=MemoryPortType.R, delay=1)
        ls.register(stg_filter, write_port_filter, read_port_filter)

    # Connect input data ports to their controllers
    for i in range(in_ports):
        id_, ag_, sg_ = in_controllers[i]
        ls.connect(input_port_list[i], id_)
        ls.connect(input_port_list[i], ag_)
        ls.connect(input_port_list[i], sg_)

    # Connect output data ports to their controllers
    for i in range(out_ports):
        id_, ag_, sg_ = out_controllers[i]
        ls.connect(output_port_list[i], id_)
        ls.connect(output_port_list[i], ag_)
        ls.connect(output_port_list[i], sg_)

    # Connect data ports to memory ports
    if dual_port:
        for p in input_port_list:
            ls.connect(p, write_mp)
        for p in output_port_list:
            ls.connect(p, read_mp)
        ls.connect(write_mp, stg)
        ls.connect(read_mp, stg)
    else:
        for p in input_port_list:
            ls.connect(p, shared_mp)
        for p in output_port_list:
            ls.connect(p, shared_mp)
        ls.connect(shared_mp, stg)

    # Connect filter ports
    if add_filter_path:
        ls.connect(in_port_filter, in_id_filter)
        ls.connect(in_port_filter, in_ag_filter)
        ls.connect(in_port_filter, in_sg_filter)

        ls.connect(out_port_filter, out_id_filter)
        ls.connect(out_port_filter, out_ag_filter)
        ls.connect(out_port_filter, out_sg_filter)

        ls.connect(in_port_filter, write_port_filter)
        ls.connect(out_port_filter, read_port_filter)

        ls.connect(write_port_filter, stg_filter)
        ls.connect(read_port_filter, stg_filter)

    return ls
