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
        'width': width
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
        'width': 32
    }

    return tech_map


def GF_Tech_Map(depth, width) -> dict:
    '''
    Currently returns the tech map for the single port SRAM, but we can
    procedurally generate different tech maps
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

    ports.append(single_port)

    tech_map = {
        'name': f"IN12LP_S1DB_W{depth:05}B{width:03}M04S2_H",
        'ports': ports,
        'depth': depth,
        'width': width
    }

    return tech_map
