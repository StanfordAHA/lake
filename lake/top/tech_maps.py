def TSMC_Tech_Map() -> dict:
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
        'name': "TS1N16FFCLLSBLVTC512X32M4S",
        'ports': ports
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
        'ports': ports
    }

    return tech_map


def GF_Tech_Map() -> dict:
    '''
    Currently returns the tech map for the single port SRAM, but we can
    procedurally generate different tech maps
    '''
    ports = []

    # TODO: FILL THESE OUT ALEX

    single_port = {
        'data_in': 'D',
        'addr': 'A',
        'write_enable': 'WEB',
        'cen': 'CEB',
        'clk': 'CLK',
        'data_out': 'Q',
        'alt_sigs': {
            # value, width
            'ALEX': (0, 2),
            'MAX': (0, 2)
        }
    }

    ports.append(single_port)

    tech_map = {
        'name': "GLOBALFOUNDRIESSTUFF",
        'ports': ports
    }

    return tech_map
