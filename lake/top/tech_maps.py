class NoValidMacroException(Exception):
    def __init__(self, *args: object) -> None:
        super().__init__(*args)


def TSMC_Tech_Map(depth, width) -> dict:
    '''
    Currently returns the tech map for the single port SRAM, but we can
    procedurally generate different tech maps
    '''
    ports = []

    single_port = {
        'write_data': 'D',
        'addr': 'A',
        'write_enable': 'WEB',
        'cen': 'CEB',
        'clk': 'CLK',
        'read_data': 'Q',
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
        'write_data': 'din0',
        'addr': 'addr0',
        'write_enable': 'web0',
        'cen': 'csb0',
        'clk': 'clk0',
        'read_data': 'dout0',
        'alt_sigs': {
            # value, width
            'wmask0': (2 ** 4 - 1, 4)
        }
    }

    # READ Port
    second_port = {
        'read_data': 'dout1',
        'addr': 'addr1',
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


def get_gf_macro_options(valid_options, depth, width):
    options_ = []
    for macro_spec, macro_vals in valid_options.items():
        word_range, word_granularity, bit_range, bit_granularity = macro_spec
        mv_, sv_ = macro_vals
        word_start, word_stop = word_range
        bit_start, bit_stop = bit_range
        # Need to go one beyond to include the end (range is exclusive end by default)
        word_opts = range(word_start, word_stop + 1, word_granularity)
        bit_opts = range(bit_start, bit_stop + 1, bit_granularity)

        if depth in word_opts and width in bit_opts:
            options_.append(macro_vals)

    print(options_)
    return options_


def GF_Tech_Map(depth, width, dual_port=False, reg_file=False,
                mux_val=8, s_val=2, hl_feat="H", pd="D") -> dict:
    '''
    Currently returns the tech map for the single port SRAM
    or the dual port SRAM (1rw1r) mux 8 ("M08" in in the inst name)
        (note that dual port mux 8 supports only the 16bit width
            width 32 uses mux 4, width 64 uses mux 2),
    but we can procedurally generate different tech maps
    '''
    ports = []

    # Density single-port SRAM
    if pd == "D":

        single_port = {
            'write_data': 'D',
            'addr': 'A',
            'write_en': 'RDWEN',
            'cen': 'CEN',
            'clk': 'CLK',
            'read_data': 'Q',
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

    # Performance single-port SRAM
    else:
        single_port = {
            'write_data': 'D',
            'addr': 'A',
            'write_en': 'RDWEN',
            'cen': 'CEN',
            'clk': 'CLK',
            'read_data': 'Q',
            'alt_sigs': {
                # value, width
                'T_LOGIC': (0, 1),
                'T_Q_RST': (0, 1),
                'MA_SAWL1': (0, 1),
                'MA_SAWL0': (0, 1),
                'MA_WL1': (0, 1),
                'MA_WL0': (0, 1),
                # 'MA_WRAS1': (0, 1),
                # 'MA_WRAS0': (0, 1),
                'MA_VD1': (0, 1),
                'MA_VD0': (0, 1),
                'MA_WRT': (0, 1),
                # 'MA_STABAS1': (0, 1),
                # 'MA_STABAS0': (0, 1),
            }
        }

    dual_port_p0rw = {
        # port RW
        'clk': 'CLK_A',
        'cen': 'CEN_A',
        'write_en': 'RDWEN_A',
        'addr': 'A_A',
        'write_data': 'D_A',
        'read_data': 'Q_A',
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
        # port R
        'clk': 'CLK_B',
        'cen': 'CEN_B',
        'addr': 'A_B',
        'read_data': 'Q_B',
        'alt_sigs': {
            # value, width
            'T_Q_RST_B': (0, 1),
            'RDWEN_B': (1, 1),
            'D_B': (0, width),
        }
    }

    rf_dual_port_p0r = {
        'clk': 'CLK_A',
        'cen': 'CEN_A',
        # 'read_en': 'RDEN_B',
        'addr': 'A_A',
        'read_data': 'Q',
        'alt_sigs': {
            # value, width
            # Only a single pin unique to this port
            'T_Q_RST_A': (0, 1),
        }
    }

    rf_dual_port_p0w = {
        'clk': 'CLK_B',
        'cen': 'CEN_B',
        # 'write_en': 'WEN_A',
        'addr': 'A_B',
        'write_data': 'D',
        'alt_sigs': {
            # value, width
            'T_LOGIC': (0, 1),
            'T_Q_RST_B': (0, 1),
            'MA_SAWL': (0, 1),
            'MA_WL': (0, 1),
            'MA_WRAS1': (0, 1),
            'MA_WRAS0': (0, 1),
            # 'MA_PRE': (0, 1),
            'MA_WRT': (0, 1),
            'MA_VD1': (0, 1),
            'MA_VD0': (0, 1),
        }
    }

    if dual_port and not reg_file:
        ports.append(dual_port_p0rw)
        ports.append(dual_port_p1r)

        # Opt for ranges instead...
        # Define everything with (bit range) (word range) -> mux, subarray
        valid_macros = {
            # word range, word granularity, bit range, bit granularity -> mux, subarray
            # MUX 2
            ((32, 512), 8, (32, 80), 2): (2, 2),
            ((256, 1024), 8, (32, 80), 2): (2, 4),
            ((512, 2048), 8, (32, 80), 2): (2, 8),

            # MUX 4
            ((64, 1024), 16, (16, 40), 2): (4, 2),
            ((512, 2048), 16, (16, 40), 2): (4, 4),
            ((1024, 4096), 16, (16, 40), 2): (4, 8),

            # MUX 8
            ((128, 2048), 32, (8, 20), 1): (8, 2),
            ((1024, 4096), 32, (8, 20), 1): (8, 4),
            ((2048, 8192), 32, (8, 20), 1): (8, 8),

            # MUX 16
            ((256, 4096), 64, (4, 10), 1): (16, 2),
            ((2048, 8192), 64, (4, 10), 1): (16, 4),
            ((4096, 16384), 64, (4, 10), 1): (16, 8),
        }

        options_ = get_gf_macro_options(valid_macros, depth, width)
        assert len(options_) > 0, f"No valid macros..."
        # Choose first for now...
        mux_val, s_val = options_[0]

        name = f"IN12LP_SDPB_W{depth:05}B{width:03}M{mux_val:02}S{s_val:01}_{hl_feat}"
    elif reg_file:

        # Define everything with (bit range) (word range) -> mux, subarray
        valid_macros = {
            # word range, word granularity, bit range, bit granularity -> mux, subarray
            # MUX 2
            ((8, 256), 8, (4, 144), 1): (2, 1),
            ((128, 512), 8, (4, 144), 1): (2, 2),

            # MUX 4
            ((16, 512), 16, (4, 72), 1): (4, 1),
            ((256, 1024), 16, (4, 72), 1): (4, 2),

        }

        options_ = get_gf_macro_options(valid_macros, depth, width)
        assert len(options_) > 0, f"No valid macros..."
        # Choose first for now...
        mux_val, s_val = options_[0]

        # ports.extend([rf_dual_port_p0r, rf_dual_port_p0w])
        ports.extend([rf_dual_port_p0w, rf_dual_port_p0r])
        name = f"IN12LP_R2PB_W{depth:05}B{width:03}M{mux_val:02}S{s_val:01}_{hl_feat}"
    else:

        if pd == "D":
            # Define everything with (bit range) (word range) -> mux, subarray
            valid_macros = {
                # word range, word granularity, bit range, bit granularity -> mux, subarray
                # MUX 4
                ((512, 2048), 16, (32, 256), 2): (4, 2),
                ((1024, 4096), 16, (32, 256), 2): (4, 4),
                ((2048, 8192), 16, (32, 256), 2): (4, 8),

                # MUX 8
                ((1024, 4096), 32, (16, 128), 1): (8, 2),
                ((2048, 8192), 32, (16, 128), 1): (8, 4),
                ((4096, 16384), 32, (16, 128), 1): (8, 8),

                # MUX 16
                ((2048, 8192), 64, (8, 64), 1): (16, 2),
                ((4096, 16384), 64, (8, 64), 1): (16, 4),
                ((8192, 32768), 64, (8, 64), 1): (16, 8),
            }
        else:
            valid_macros = {
                # word range, word granularity, bit range, bit granularity -> mux, subarray
                # MUX 4
                ((512, 2048), 16, (32, 192), 2): (4, 2),
                ((1024, 4096), 16, (32, 192), 2): (4, 4),
                ((2048, 8192), 16, (32, 192), 2): (4, 8),

                # MUX 8
                ((1024, 4096), 32, (16, 96), 1): (8, 2),
                ((2048, 8192), 32, (16, 96), 1): (8, 4),
                ((4096, 16384), 32, (16, 96), 1): (8, 8),

                # MUX 16
                ((2048, 8192), 64, (8, 48), 1): (16, 2),
                ((4096, 16384), 64, (8, 48), 1): (16, 4),
                ((8192, 32768), 64, (8, 48), 1): (16, 8),
            }

        options_ = get_gf_macro_options(valid_macros, depth, width)
        assert len(options_) > 0, f"No valid macros..."
        # Choose first for now...
        mux_val, s_val = options_[0]

        ports.append(single_port)
        name = f"IN12LP_S1{pd}B_W{depth:05}B{width:03}M{mux_val:02}S{s_val:01}_{hl_feat}"

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
        'write_data': 'din',
        'addr': 'adr',
        'write_en': 'wen',
        'read_en': 'ren',
        # 'cen': 'CEB',
        'clk': 'clk',
        'read_data': 'q',
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
