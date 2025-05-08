from lake.top.memory_controller import MemoryController
from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ReadyValidScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort
from lake.top.tech_maps import GF_Tech_Map


def build_four_port_wide_fetch_rv(storage_capacity=16384, data_width=16, dims: int = 6, vec_width=4, physical=True,
                                  reg_file=False, vec_capacity=2, opt_rv=True, remote_storage=True, id_width=16) -> Spec:

    # TODO: Override this in garnet and not here...
    id_width = 11

    ls = Spec(name="lakespec_mem", opt_rv=opt_rv, remote_storage=remote_storage, run_flush_pass=False,
              config_passthru=True, comply_17=True)

    # Don't opt timing on the in ports (which really just adds a fifo at the input which we don't need)
    in_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                   vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.IN,
                   opt_rv=opt_rv, opt_timing=False)
    in_port2 = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                    vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.IN,
                    opt_rv=opt_rv, opt_timing=False)
    out_port = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                    vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.OUT,
                    opt_rv=opt_rv)
    out_port2 = Port(ext_data_width=data_width, int_data_width=data_width * vec_width,
                     vec_capacity=vec_capacity, runtime=Runtime.DYNAMIC, direction=Direction.OUT,
                     opt_rv=opt_rv)

    ls.register(in_port, in_port2, out_port, out_port2)

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

    data_bytes = (data_width * vec_width) // 8
    tech_map = None
    if physical:
        tech_map = GF_Tech_Map(depth=storage_capacity // data_bytes, width=data_width * vec_width, dual_port=False)

    # 1024 Bytes
    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map)
    shared_rw_mem_port = MemoryPort(data_width=data_width * vec_width, mptype=MemoryPortType.RW, delay=1)
    ls.register(stg, shared_rw_mem_port)

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

    stg = SingleBankStorage(capacity=storage_capacity, tech_map=tech_map)

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

    def get_bitstream(self, config_json, prefix=""):
        print("in spec config bitstream...")
        print(config_json)
        bs = self.spec.gen_bitstream(config_json, override=True)
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
