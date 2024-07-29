import kratos as kts
from lake.utils.spec_enum import Direction, Runtime
from lake.spec.memory_port import MemoryPort
from lake.spec.component import Component
from lake.utils.util import shift_reg, inline_multiplexer, round_up_to_power_of_2
from lake.spec.reg_fifo import RegFIFO
from kratos import always_comb


class MemoryInterfaceDecoder(Component):

    # def __init__(self, name: str, debug: bool = False, is_clone: bool = False, internal_generator=None):
    def __init__(self, name: str = None, port_type: Direction = None, port_intf: dict = None,
                 memports=None, runtime=Runtime.STATIC):
        super().__init__(name=name)
        # super().__init__(name, debug, is_clone, internal_generator)
        assert memports is not None
        self.p_intf = None
        self.mp_intf = {}
        self.memports = memports
        self.port_intf = port_intf
        self.port_direction = port_type
        self.addr_ranges = None
        self.runtime = runtime
        # Set the delay to be the worst case delay over all memports
        # For now they are always all the same...
        self.delay = 0
        for memport in self.memports:
            new_delay = memport.get_port_delay()
            if new_delay > self.delay:
                self.delay = new_delay
        print(f"Final delay is {self.delay}")

    def get_delay(self):
        return self.delay

    def gen_hardware(self):

        # Memory port needs to output a resource ready to
        self._resource_ready = self.output(name="resource_ready", width=1)
        self._resource_ready_lcl = self.var(name="resource_ready_lcl", width=1)

        # Kratos won't let you use an output variable locally as of now
        self.wire(self._resource_ready, self._resource_ready_lcl)

        self.p_intf = {}
        self.p_intf['direction'] = self.port_direction

        if self.runtime == Runtime.DYNAMIC:
            self.p_intf['grant'] = self.output("grant", 1)
            self._grant_lcl = self.var("grant_lcl", 1)

        # Create the port facing side
        if self.port_direction == Direction.IN:
            self.p_intf['addr'] = self.input("addr", self.port_intf['addr'].width)
            self.p_intf['data'] = self.input("data", self.port_intf['data'].width)
            self.p_intf['en'] = self.input("en", 1)
            self.wire(self._resource_ready_lcl, kts.const(1, 1))

        elif self.port_direction == Direction.OUT:
            self.p_intf['addr'] = self.input("addr", self.port_intf['addr'].width)
            self.p_intf['en'] = self.input("en", 1)

            # if self.runtime == Runtime.DYNAMIC:
            data_to_port_rv = self.rvoutput(name="data", width=self.port_intf['data'].width, packed=True)
            self.p_intf['data'] = data_to_port_rv.get_port()

            muxed_data_in = self.var("chosen_read", self.port_intf['data'].width, packed=True)
            # all_data_in = self.var("all_memports_in", self.port_intf['data'].width, size=len(self.memports))
            all_data_in = [self.var(f"all_data_in_{j}", self.port_intf['data'].width) for j in range(len(self.memports))]
            shift_reg_dec_en_in = self.var("shift_reg_dec_en_in", len(self.memports))
            shift_reg_dec_en_out = shift_reg(self, shift_reg_dec_en_in, chain_depth=self.delay)
            # shift_reg_dec_en_out = None

            # Mux the data based on the decoded enable signal shifted
            inline_multiplexer(self, f"muxed_data_from_memports", sel=shift_reg_dec_en_out, one=muxed_data_in, many=all_data_in)

            # If we have a dynamic port, we need to shift the specific enable as well as valid through a shift reg
            if self.runtime == Runtime.DYNAMIC:
                # In this case, we need to instantiate a FIFO as a skid buffer to accomodate
                # the latency of the physical storage in addition to a shift register chain

                rupp2 = round_up_to_power_of_2(2 + self.delay)

                reg_fifo = RegFIFO(self.port_intf['data'].width, 1, rupp2, almost_full_diff=self.delay + 1)
                # shift_reg_out = None
                # shift_register_chain = None
                # Shift register on the actual transaction
                shift_reg_en_in = self.var("shift_reg_en_in", 1)
                shift_reg_en_out = shift_reg(self, shift_reg_en_in & self._grant_lcl, chain_depth=self.delay)
                # Shift register on the decoded enable so we know which to use (one-hot mux sel)

                self.add_child(f"reg_fifo",
                                reg_fifo,
                                clk=self._clk,
                                rst_n=self._rst_n,
                                # clk_en=self._clk_en,
                                clk_en=kts.const(1, 1),
                                # Only push the request if enable is high and the grant is high
                                push=shift_reg_en_out,
                                # push=shift_reg_en_out,
                                pop=data_to_port_rv.get_ready(),
                                data_in=muxed_data_in,
                                data_out=data_to_port_rv.get_port())

                # Valid going out for the data is the not empty of the fifo
                self.wire(data_to_port_rv.get_valid(), ~reg_fifo.ports.empty)
                # # The shift reg just captures if an actual even occured which is the enable in +
                # # it needs to be qualified on the almost_full from the fifo - since we sized this for appropriate
                # # amount of skid buffering, we don't need to worry about the FIFO's full signal
                # shift_reg_in = en_from_port & ~reg_fifo.ports.almost_full
                self.wire(shift_reg_en_in, self.p_intf['en'] & ~reg_fifo.ports.almost_full)
                self.wire(self._resource_ready_lcl, ~reg_fifo.ports.almost_full)

            else:

                shift_reg_en_in = self.var("shift_reg_en_in", 1)
                self.wire(shift_reg_en_in, self.p_intf['en'])
                shift_reg_en_out = shift_reg(self, shift_reg_en_in, chain_depth=self.delay)

                self.wire(data_to_port_rv.get_valid(), shift_reg_en_out)
                self.wire(self._resource_ready_lcl, kts.const(1, 1))
                self.wire(data_to_port_rv.get_port(), muxed_data_in)
            # else:
                # self.p_intf['data'] = self.output("data", self.port_intf['data'].width)
        else:
            raise NotImplementedError

        self.addr_ranges = []
        base = 0
        # This allows us to figure out the address ranges that are associated with each enable
        for i_, mp in enumerate(self.memports):
            mp: MemoryPort
            addr_range = mp.get_num_addrs()
            self.addr_ranges.append((base, base + addr_range - 1))

            self.mp_intf[i_] = {}
            self.mp_intf[i_]['direction'] = self.port_direction

            # Create the MemoryPort facing side
            self.mp_intf[i_]['addr'] = self.output(f"mp_addr_{i_}", kts.clog2(mp.get_num_addrs()))
            self.mp_intf[i_]['en_lcl'] = self.var(f"mp_en_lcl_{i_}", 1)
            self.mp_intf[i_]['en'] = self.output(f"mp_en_{i_}", 1)

            self.wire(self.mp_intf[i_]['en'], self.mp_intf[i_]['en_lcl'])

            if self.runtime == Runtime.DYNAMIC:
                self.mp_intf[i_]['grant'] = self.input(f"mp_grant_{i_}", 1)

            if self.port_direction == Direction.IN:
                self.mp_intf[i_]['data'] = self.output(f"mp_data_{i_}", self.port_intf['data'].width)
                self.wire(self.mp_intf[i_]['data'], self.p_intf['data'])
            elif self.port_direction == Direction.OUT:
                self.mp_intf[i_]['data'] = self.input(f"mp_data_{i_}", self.port_intf['data'].width)
            else:
                raise NotImplementedError

            # # This actually needs to be based on the enable
            # self.wire(self.p_intf['data'], self.mp_intf[i_]['data'])

            if self.port_direction == Direction.IN:
                # Now decode
                @always_comb
                def decode_en():
                    # The address to send to a specific memory port just needs the base subtracted away
                    self.mp_intf[i_]['addr'] = self.p_intf['addr'] - base
                    # The enable will be high if the address is in the range of the memport
                    self.mp_intf[i_]['en_lcl'] = 0
                    if (self.p_intf['addr'] >= base) and (self.p_intf['addr'] <= (base + addr_range - 1)):
                        # self.mp_intf[i_]['en'] = 1
                        self.mp_intf[i_]['en_lcl'] = self.p_intf['en']
            elif self.port_direction == Direction.OUT:
                @always_comb
                def decode_en():
                    # The address to send to a specific memory port just needs the base subtracted away
                    self.mp_intf[i_]['addr'] = self.p_intf['addr'] - base
                    all_data_in[i_] = self.mp_intf[i_]['data']
                    # The enable will be high if the address is in the range of the memport
                    self.mp_intf[i_]['en_lcl'] = 0
                    if (self.p_intf['addr'] >= base) and (self.p_intf['addr'] <= (base + addr_range - 1)):
                        # self.mp_intf[i_]['en'] = 1
                        self.mp_intf[i_]['en_lcl'] = self.p_intf['en']

            self.add_code(decode_en)
            base = base + addr_range

        if self.port_direction == Direction.OUT:
            # Now finally hook up the final decoded enable signal
            self.wire(shift_reg_dec_en_in, kts.concat(*[self.mp_intf[i_]['en_lcl'] for i_ in range(len(self.memports))]))

        # Need to OR together all the grants...
        if self.runtime == Runtime.DYNAMIC:
            all_grants = [self.mp_intf[i_]['grant'] for i_ in range(len(self.memports))]
            self.wire(self._grant_lcl, kts.concat(*all_grants).r_or())
            self.wire(self.p_intf['grant'], self._grant_lcl)
            # self.wire(self.p_intf['grant'], kts.concat(*all_grants).r_or())

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def get_p_intf(self):
        return self.p_intf

    def get_mp_intf(self):
        return self.mp_intf
