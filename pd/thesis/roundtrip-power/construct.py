#=========================================================================
# construct.py -- lean per-tile clockwork round-trip power graph
#=========================================================================
# Standalone lake graph the garnet sweep drives for `--per-tile`. Instead of
# rebuilding RTL/synth/SRAM (garnet already did), a `setup` step stages the
# spec's synthesized netlist + SRAM + testbench + collateral (from the parent
# garnet workspace), and this graph runs:
#
#   setup -> verify-lake-collateral -> clockwork-roundtrip-compile
#         -> clockwork-roundtrip-sim-synth -> vcd2saif -> ptpx-synth
#
# i.e. compile the app against the spec collateral, split into per-memtile
# configs, simulate each memtile in isolation on its exact access pattern
# (self-checked against golden), and report power on the synthesized netlist.
#
# The round-trip step dirs are the SAME ones construct-commercial-full.py uses
# (single owner of the round-trip flow in lake); this construct just wires the
# lean synth-level path and sources upstream artifacts from `setup`. Sibling
# step dirs live one level up (pd/thesis/), hence the '/../' prefixes.

import os

from mflowgen.components import Graph, Step


def construct(**kwargs):

    g = Graph()

    #---------------------------------------------------------------------
    # Parameters
    #---------------------------------------------------------------------
    adk_name = 'gf12-adk'
    adk_view = 'view-standard'

    parameters = {
        'construct_path' : __file__,
        'design_name'    : os.environ.get('design_name', 'lakespec'),
        'clock_period'   : float(os.environ.get('clock_period', 1000)),
        'adk'            : adk_name,
        'adk_view'       : adk_view,
        # round-trip sim spec params (mirror clockwork-roundtrip-sim-synth
        # defaults; the garnet driver overrides these from the spec via env).
        'storage_capacity'  : int(os.environ.get('storage_capacity', 8192)),
        'data_width'        : int(os.environ.get('data_width', 16)),
        'fetch_width'       : int(os.environ.get('fetch_width', 4)),
        'dimensionality'    : int(os.environ.get('dimensionality', 6)),
        'in_ports'          : int(os.environ.get('in_ports', 2)),
        'out_ports'         : int(os.environ.get('out_ports', 2)),
        'dual_port'         : os.environ.get('dual_port', 'False'),
        'vec_capacity'      : int(os.environ.get('vec_capacity', 2)),
        # ptpx strip_path into the round-trip testbench hierarchy.
        'strip_path'     : os.environ.get('strip_path', 'tb/dut'),
        'saif_instance'  : os.environ.get('saif_instance', 'tb/dut'),
    }
    for key, value in kwargs.items():
        parameters[key] = value

    this_dir = os.path.dirname(os.path.abspath(__file__))
    pd_thesis = this_dir + '/..'   # sibling round-trip step dirs live here

    #---------------------------------------------------------------------
    # Nodes
    #---------------------------------------------------------------------
    g.set_adk(adk_name)
    adk = g.get_adk_step()

    setup    = Step(this_dir  + '/setup')
    verify   = Step(pd_thesis + '/verify-lake-collateral')
    compile_ = Step(pd_thesis + '/clockwork-roundtrip-compile')
    sim      = Step(pd_thesis + '/clockwork-roundtrip-sim-synth')

    gen_saif = Step('synopsys-vcd2saif-convert', default=True)
    gen_saif.set_name('synopsys-vcd2saif-convert-roundtrip-synth')

    pt_power = Step(pd_thesis + '/synopsys-ptpx-synth')
    pt_power.set_name('synopsys-ptpx-synth-roundtrip-synth')

    #---------------------------------------------------------------------
    # Graph -- add
    #---------------------------------------------------------------------
    g.add_step(setup)
    g.add_step(verify)
    g.add_step(compile_)
    g.add_step(sim)
    g.add_step(gen_saif)
    g.add_step(pt_power)

    #---------------------------------------------------------------------
    # setup provides the garnet-built artifacts; SRAM db feeds power.
    #---------------------------------------------------------------------
    sim.extend_inputs(['sram.v'])
    pt_power.extend_inputs(['sram_tt.db'])

    #---------------------------------------------------------------------
    # Graph -- connect
    #---------------------------------------------------------------------
    # Collateral cross-check: setup (context A + spec) -> verify -> compile.
    g.connect(setup.o('lake_collateral.json'), verify.i('lake_collateral.json'))
    g.connect(setup.o('spec_config.json'),     verify.i('spec_config.json'))
    g.connect(verify.o('lake_collateral.json'),
              compile_.i('lake_collateral.json'))

    # Compile app -> per-memtile configs -> synth-netlist sim.
    g.connect(compile_.o('map_results'), sim.i('map_results'))
    g.connect_by_name(adk,   sim)
    g.connect(setup.o('design.v'),     sim.i('design.v'))      # synth netlist
    g.connect(setup.o('sram.v'),       sim.i('sram.v'))
    g.connect(setup.o('testbench.sv'), sim.i('testbench.sv'))

    # Sim VCD -> SAIF -> PrimeTime-PX on the synth netlist.
    g.connect_by_name(sim,      gen_saif)
    g.connect_by_name(gen_saif, pt_power)
    g.connect_by_name(adk,      pt_power)
    g.connect(setup.o('design.v'),    pt_power.i('design.v'))
    g.connect(setup.o('design.sdc'),  pt_power.i('design.sdc'))
    g.connect(setup.o('design.spef'), pt_power.i('design.spef'))
    g.connect(setup.o('sram_tt.db'),  pt_power.i('sram_tt.db'))

    #---------------------------------------------------------------------
    # Parameters
    #---------------------------------------------------------------------
    g.update_params(parameters)

    return g


if __name__ == '__main__':
    g = construct()
