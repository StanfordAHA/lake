def TSMC_Tech_Map(depth, width) -> dict:
    '''
    Currently returns the tech map for the single port SRAM, but we can
    procedurally generate different tech maps
    '''
    ports = []

    single_port = {
        'data_in': 'D',
        'addr': 'A',
        'write_enable': 'WEB',
        'cen': 'CEB',
        'clk': 'CLK',
        'data_out': 'Q',
        'alt_sigs': {
            # value, width
            'RTSEL': (0, 2),
            'WTSEL': (0, 2)
        }
    }

    ports.append(single_port)

    tech_map = {
        'name': f"TS1N16FFCLLSBLVTC{depth}X{width}M4S",
        'ports': ports,
        'depth': depth,
        'width': width,
        'active': 'low'
    }

    return tech_map


def SKY_Tech_Map() -> dict:
    '''
    Currently returns the tech map for the sky130 dual port SRAM, but we can
    procedurally generate different tech maps

    NOTE - Ordering of ports matters!!!
    '''
    ports = []

    # READWRITE Port
    first_port = {
        'data_in': 'din0',
        'addr': 'addr0',
        'write_enable': 'web0',
        'cen': 'csb0',
        'clk': 'clk0',
        'data_out': 'dout0',
        'alt_sigs': {
            # value, width
            'wmask0': (2 ** 4 - 1, 4)
        }
    }

    # READ Port
    second_port = {
        'data_out': 'dout1',
        'read_addr': 'addr1',
        'cen': 'csb1',
        'clk': 'clk1',
        'alt_sigs': {}
    }

    ports.append(first_port)
    ports.append(second_port)

    tech_map = {
        'name': "sky130_sram_1kbyte_1rw1r_32x256_8",
        'ports': ports,
        'depth': 256,
        'width': 32,
        'active': 'low'
    }

    return tech_map


def GF_Tech_Map(depth, width, dual_port=False) -> dict:
    '''
    Currently returns the tech map for the single port SRAM
    or the dual port SRAM (1rw1r) mux 8 ("M08" in in the inst name)
        (note that dual port mux 8 supports only the 16bit width
            width 32 uses mux 4, width 64 uses mux 2),
    but we can procedurally generate different tech maps
    '''
    ports = []

    single_port = {
        'data_in': 'D',
        'addr': 'A',
        'write_enable': 'RDWEN',
        'cen': 'CEN',
        'clk': 'CLK',
        'data_out': 'Q',
        'alt_sigs': {
            # value, width
            'T_LOGIC': (0, 1),
            'T_Q_RST': (0, 1),
            'MA_SAWL1': (0, 1),
            'MA_SAWL0': (0, 1),
            'MA_WL1': (0, 1),
            'MA_WL0': (0, 1),
            'MA_WRAS1': (0, 1),
            'MA_WRAS0': (0, 1),
            'MA_VD1': (0, 1),
            'MA_VD0': (0, 1),
            'MA_WRT': (0, 1),
            'MA_STABAS1': (0, 1),
            'MA_STABAS0': (0, 1),
        }
    }

    dual_port_p0rw = {
        # port RW
        'clk': 'CLK_A',
        'cen': 'CEN_A',
        'write_enable': 'RDWEN_A',
        'addr': 'A_A',
        'data_in': 'D_A',
        'data_out': 'Q_A',
        'alt_sigs': {
            # value, width
            'T_Q_RST_A': (0, 1),
            'T_LOGIC': (0, 1),
            'MA_SAWL1': (0, 1),
            'MA_SAWL0': (0, 1),
            'MA_WL1': (0, 1),
            'MA_WL0': (0, 1),
            'MA_WRAS1': (0, 1),
            'MA_WRAS0': (0, 1),
            'MA_VD1': (0, 1),
            'MA_VD0': (0, 1),
            'MA_WRT': (0, 1),
        }
    }

    dual_port_p1r = {
        # port RW
        'clk': 'CLK_B',
        'cen': 'CEN_B',
        'read_addr': 'A_B',
        'data_out': 'Q_B',
        'alt_sigs': {
            # value, width
            'T_Q_RST_B': (0, 1),
            'RDWEN_B': (1, 1),
            'D_B': (0, width),
        }
    }

    if dual_port:
        ports.append(dual_port_p0rw)
        ports.append(dual_port_p1r)
        name = f"IN12LP_SDPB_W{depth:05}B{width:03}M08S2_H"
    else:
        ports.append(single_port)
        name = f"IN12LP_S1DB_W{depth:05}B{width:03}M04S2_H"

    tech_map = {
        'name': name,
        'ports': ports,
        'depth': depth,
        'width': width,
        'active': 'low'
    }

    return tech_map


def Intel_Tech_Map(depth, width,
                   async_reset=None,
                   compiler_name='ip224uhdlp1p11rf',
                   column_mux=4,
                   bank_count=2,
                   center_decode=1,
                   bit_write_enabled=0,
                   dfx_setting=0,
                   redundancy_setting=0,
                   power_management=0,
                   dual_supply=0,
                   assist_setting=1,
                   arr_prog_timing=1,
                   vt_setting='h') -> dict:
    '''
    Currently returns the tech map for the single port SRAM, but we can
    procedurally generate different tech maps
    '''
    ports = []

    # From slide
    assert column_mux in [4, 8, 16]
    assert bank_count in [2, 4, 8]
    assert center_decode == 1
    assert bit_write_enabled in [0, 1]
    assert dfx_setting == 0
    assert redundancy_setting in [0, 1]
    assert power_management in [0, 1]
    assert dual_supply == 0
    assert assist_setting == 1
    assert arr_prog_timing == 1
    assert vt_setting == 'h'
    assert async_reset is not None

    single_port = {
        'data_in': 'din',
        'addr': 'adr',
        'write_enable': 'wen',
        'read_enable': 'ren',
        # 'cen': 'CEB',
        'clk': 'clk',
        'data_out': 'q',
        'alt_sigs': {
            # value, width
            'fwen': (async_reset, 1),
            'mcen': (0, 1),
            'mc': (0, 3),
            'wpulseen': (0, 1),
            'clkbyp': (0, 1),
            'wpulse': (0, 2),
            'wa': (0, 2),
        }
    }

    ports.append(single_port)

    tech_map = {
        'name': f"{compiler_name}_{depth}x{width}m{column_mux}b{bank_count}c{center_decode}s{bit_write_enabled}_t{dfx_setting}r{redundancy_setting}p{power_management}d{dual_supply}a{assist_setting}m{arr_prog_timing}{vt_setting}",
        'ports': ports,
        'depth': depth,
        'width': width,
        'active': 'high'
    }

    return tech_map
