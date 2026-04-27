#=========================================================================
# construct.py
#=========================================================================
# Demo with 16-bit GcdUnit
#
# Author : Christopher Torng
# Date   : June 2, 2019
#

import os
import re

from mflowgen.components import Graph, Step
from lake.top.tech_maps import GF_Tech_Map

def construct(**kwargs):

  g = Graph()

  #-----------------------------------------------------------------------
  # Parameters
  #-----------------------------------------------------------------------

  # adk_name = 'freepdk-45nm'
  adk_name = 'gf12-adk'
  adk_view = 'view-standard'

  parameters = {
    'construct_path' : __file__,
    'design_name'    : 'lakespec',
    'clock_period'   : 1000,
    'adk'            : adk_name,
    'adk_view'       : adk_view,
    'topographical'  : True,
    'saif_instance'  : 'GcdUnitTb/GcdUnit_inst'
  }

  for key, value in kwargs.items():
    parameters[key] = value

  #-----------------------------------------------------------------------
  # Create nodes
  #-----------------------------------------------------------------------

  this_dir = os.path.dirname( os.path.abspath( __file__ ) )

  # ADK step

  g.set_adk( adk_name )
  adk = g.get_adk_step()

  # Custom steps

  rtl       = Step( this_dir + '/rtl' )
  testbench = Step( this_dir + '/testbench')
  gen_sram  = Step( this_dir + '/gen_sram_macro'               )

  # Default steps
  def default_step(step_name):
      return Step(step_name, default=True)

  info           = Step( 'info',                           default=True )
  constraints    = Step( this_dir + '/constraints',        )
  custom_genus_scripts    = Step( this_dir + '/custom-genus-scripts',        )
  synth          = default_step('cadence-genus-synthesis')         # noqa
  iflow          = Step( 'cadence-innovus-flowsetup',      default=True )
  init           = Step( this_dir + '/cadence-innovus-init')
  # init           = Step( 'cadence-innovus-init', default=True)
  power          = Step( 'cadence-innovus-power',          default=True )
  place          = Step( 'cadence-innovus-place',          default=True )
  cts            = Step( 'cadence-innovus-cts',            default=True )
  postcts_hold   = Step( 'cadence-innovus-postcts_hold',   default=True )
  route          = Step( 'cadence-innovus-route',          default=True )
  postroute      = Step( 'cadence-innovus-postroute',      default=True )
  postroute_hold = Step( 'cadence-innovus-postroute_hold', default=True )
  signoff        = Step( this_dir + '/cadence-innovus-signoff')
  pt_signoff     = Step( 'synopsys-pt-timing-signoff',     default=True )
  genlibdb       = Step( 'synopsys-ptpx-genlibdb',         default=True )
  gdsmerge       = Step( 'mentor-calibre-gdsmerge',        default=True )
  drc            = Step( 'mentor-calibre-drc',             default=True )
  lvs            = Step( 'mentor-calibre-lvs',             default=True )
  debugcalibre   = Step( 'cadence-innovus-debug-calibre',  default=True )
  vcs_sim        = Step( this_dir + '/synopsys-vcs-sim')
  vcs_sim_rtl        = Step( this_dir + '/synopsys-vcs-sim-rtl')
  power_est      = Step( 'synopsys-pt-power',              default=True )
  formal_verif   = Step( 'synopsys-formality-verification', default=True )
  gen_saif       = Step('synopsys-vcd2saif-convert', default=True)
  # Second vcd2saif instance: feeds RTL-sim VCD to synth-level PT power.
  # Lets synth-level power run without going through full PnR.
  gen_saif_rtl   = Step('synopsys-vcd2saif-convert', default=True)
  gen_saif_rtl.set_name('synopsys-vcd2saif-convert-rtl')
  pt_power_synth    = Step( this_dir + '/synopsys-ptpx-synth')
  pt_power_gl = Step(this_dir + '/synopsys-ptpx-gl')

  print(f"Extending LVS inputs...")
  lvs.extend_inputs(['sram.spi'])

  order = synth.get_param('order')
  order.append('copy_sdc.tcl')
  synth.set_param('order', order)

  # Compute SRAM macro parameters from design parameters and set them on
  # the gen_sram_macro step so gen_srams.sh compiles the correct macro.
  # Parameters are passed in via --graph-kwargs from create_mflowgen_experiments.py.
  _sc   = parameters.get('storage_capacity', 8192)   # bytes
  _dw   = parameters.get('data_width', 16)            # bits
  _fw   = parameters.get('fetch_width', 4)            # words per SRAM port
  _dp   = parameters.get('dual_port', False)

  _data_bytes = _dw // 8
  _sram_depth = _sc // _data_bytes // _fw
  _sram_width = _dw * _fw

  _tech_map = GF_Tech_Map(depth=_sram_depth, width=_sram_width, dual_port=_dp)
  _m = re.search(r'W(\d+)B(\d+)M(\d+)S(\d+)_', _tech_map['name'])
  if _m:
    gen_sram.set_param('num_words',     int(_m.group(1)))
    gen_sram.set_param('word_size',     int(_m.group(2)))
    gen_sram.set_param('mux_size',      int(_m.group(3)))
    gen_sram.set_param('num_subarrays', int(_m.group(4)))
    print(f"gen_sram_macro: {_tech_map['name']} "
          f"(num_words={_m.group(1)}, word_size={_m.group(2)}, "
          f"mux_size={_m.group(3)}, num_subarrays={_m.group(4)})")
  else:
    raise RuntimeError(f"Could not parse SRAM macro name: {_tech_map['name']}")

  rtl.set_param('python_command', parameters.get('python_command', ''))
  rtl.set_param('test_dir',       parameters.get('test_dir', ''))
  rtl.set_param('storage_capacity', parameters.get('storage_capacity', 8192))
  rtl.set_param('data_width',       parameters.get('data_width', 16))
  rtl.set_param('fetch_width',      parameters.get('fetch_width', 4))
  rtl.set_param('dimensionality',   parameters.get('dimensionality', 6))
  rtl.set_param('in_ports',         parameters.get('in_ports', 2))
  rtl.set_param('out_ports',        parameters.get('out_ports', 2))
  rtl.set_param('dual_port',        parameters.get('dual_port', False))

  #-----------------------------------------------------------------------
  # Modify Nodes
  #-----------------------------------------------------------------------

  verif_post_synth = formal_verif.clone()
  verif_post_synth.set_name('verif_post_synth')
  verif_post_layout = formal_verif.clone()
  verif_post_layout.set_name('verif_post_layout')

  #-----------------------------------------------------------------------
  # Graph -- Add nodes
  #-----------------------------------------------------------------------

  g.add_step( info           )
  g.add_step( rtl            )
  g.add_step( gen_sram       )
  g.add_step( constraints    )
  g.add_step( custom_genus_scripts             )
  g.add_step( synth             )
  g.add_step( iflow          )
  g.add_step( init           )
  g.add_step( power          )
  g.add_step( place          )
  g.add_step( cts            )
  g.add_step( postcts_hold   )
  g.add_step( route          )
  g.add_step( postroute      )
  g.add_step( postroute_hold )
  g.add_step( signoff        )
  g.add_step( pt_signoff     )
  g.add_step( genlibdb       )
  g.add_step( gdsmerge       )
  g.add_step( drc            )
  g.add_step( lvs            )
  g.add_step( debugcalibre   )
  g.add_step( testbench      )
  g.add_step( vcs_sim        )
  g.add_step( vcs_sim_rtl    )
  g.add_step( power_est      )
  g.add_step( verif_post_synth )
  g.add_step( verif_post_layout )
  g.add_step( gen_saif    )
  g.add_step( gen_saif_rtl )
  g.add_step( pt_power_synth    )
  g.add_step(pt_power_gl)

  synth.extend_inputs( ['sram_tt.lib', 'sram.lef', 'sram_tt.db'] )
  synth.extend_inputs(custom_genus_scripts.all_outputs())
  synth.extend_outputs(["sdc"])

  hier_steps = [ iflow, init, power, place, cts, postcts_hold, route, postroute, signoff]#, vcs_sim]

  for step in hier_steps:
    step.extend_inputs( ['sram_tt.lib', 'sram.lef'] )

  # vcs_sim.extend_inputs(['sram.v', 'design.v'])
  vcs_sim.extend_inputs(['sram.v'])
  vcs_sim_rtl.extend_inputs(['sram.v'])

  pt_power_synth.extend_inputs(['sram_tt.db'])
  pt_power_gl.extend_inputs(['sram_tt.db'])

  #-----------------------------------------------------------------------
  # Graph -- Add edges
  #-----------------------------------------------------------------------

  # Connect by name

  g.connect_by_name( adk,      gen_sram       )
  g.connect_by_name( adk,            synth             )
  g.connect_by_name( adk,            iflow          )
  g.connect_by_name( adk,            init           )
  g.connect_by_name( adk,            power          )
  g.connect_by_name( adk,            place          )
  g.connect_by_name( adk,            cts            )
  g.connect_by_name( adk,            postcts_hold   )
  g.connect_by_name( adk,            route          )
  g.connect_by_name( adk,            postroute      )
  g.connect_by_name( adk,            postroute_hold )
  g.connect_by_name( adk,            signoff        )
  g.connect_by_name( adk,            pt_signoff        )
  g.connect_by_name( adk,            gdsmerge       )
  g.connect_by_name( adk,            drc            )
  g.connect_by_name( adk,            lvs            )
  g.connect_by_name( adk,             pt_power_synth    )
  g.connect_by_name(adk, pt_power_gl)

  g.connect_by_name( rtl,            synth             )
  g.connect_by_name( rtl,            gen_sram             )
  g.connect_by_name( constraints,    synth             )
  g.connect_by_name( custom_genus_scripts,    synth             )

  g.connect_by_name( synth,             iflow          )
  g.connect_by_name( synth,             init           )
  g.connect_by_name( synth,             power          )
  g.connect_by_name( synth,             place          )
  g.connect_by_name( synth,             cts            )
  g.connect_by_name( synth,              pt_power_synth    ) # design.namemap
  # g.connect_by_name( gen_saif,              synth    )
  # Synth-level power gets activity from the RTL-sim VCD (no PnR required).
  # Post-PnR power (pt_power_gl) gets activity from the GLS-sim VCD.
  g.connect_by_name( gen_saif_rtl, pt_power_synth )
  g.connect_by_name( gen_saif,     pt_power_gl    )

  g.connect_by_name( vcs_sim,       gen_saif     )
  g.connect_by_name( vcs_sim_rtl,   gen_saif_rtl )

  g.connect_by_name( iflow,          init           )
  g.connect_by_name( iflow,          power          )
  g.connect_by_name( iflow,          place          )
  g.connect_by_name( iflow,          cts            )
  g.connect_by_name( iflow,          postcts_hold   )
  g.connect_by_name( iflow,          route          )
  g.connect_by_name( iflow,          postroute      )
  g.connect_by_name( iflow,          postroute_hold )
  g.connect_by_name( iflow,          signoff        )

  g.connect_by_name( init,           power          )
  g.connect_by_name( power,          place          )
  g.connect_by_name( place,          cts            )
  g.connect_by_name( cts,            postcts_hold   )
  g.connect_by_name( postcts_hold,   route          )
  g.connect_by_name( route,          postroute      )
  g.connect_by_name( postroute,      postroute_hold )
  g.connect_by_name( postroute_hold, signoff        )

  g.connect_by_name( signoff,        pt_signoff     )

  g.connect_by_name( signoff,        genlibdb       )
  g.connect_by_name( adk,            genlibdb       )

  g.connect_by_name( signoff,        gdsmerge       )

  g.connect_by_name( signoff,        drc            )
  g.connect_by_name( gdsmerge,       drc            )
  g.connect_by_name( signoff,        lvs            )
  g.connect_by_name( gdsmerge,       lvs            )

  g.connect_by_name( adk,            debugcalibre   )
  g.connect_by_name( synth,             debugcalibre   )
  g.connect_by_name( iflow,          debugcalibre   )
  g.connect_by_name( signoff,        debugcalibre   )
  g.connect_by_name( drc,            debugcalibre   )
  g.connect_by_name( lvs,            debugcalibre   )

  g.connect_by_name( adk,            vcs_sim        )
  g.connect_by_name( adk,            vcs_sim_rtl    )
  # g.connect_by_name( signoff,        vcs_sim        )


  g.connect_by_name( adk,            power_est      )
  g.connect_by_name( signoff,        power_est      )
  g.connect_by_name( vcs_sim,        power_est      )
  g.connect_by_name( gen_sram,        vcs_sim       )
  g.connect_by_name( gen_sram,        vcs_sim_rtl   )

  g.connect_by_name( gen_sram,      synth          )
  g.connect_by_name( gen_sram,      iflow          )
  g.connect_by_name( gen_sram,      init           )
  g.connect_by_name( gen_sram,      power          )
  g.connect_by_name( gen_sram,      place          )
  g.connect_by_name( gen_sram,      cts            )
  g.connect_by_name( gen_sram,      postcts_hold   )
  g.connect_by_name( gen_sram,      route          )
  g.connect_by_name( gen_sram,      postroute      )
  g.connect_by_name( gen_sram,      postroute_hold )
  g.connect_by_name( gen_sram,      signoff        )
  g.connect_by_name( gen_sram,     pt_power_synth     )
  g.connect_by_name(gen_sram, pt_power_gl)
  # g.connect_by_name( gen_sram,      genlibdb       )
  # g.connect_by_name( gen_sram,      pt_signoff     )
  g.connect_by_name( gen_sram,      drc            )
  # g.connect_by_name( gen_sram,      drc_pm         )
  g.connect_by_name( gen_sram,      lvs            )

  g.connect_by_name(signoff, pt_power_gl)

  g.connect_by_name( adk,            verif_post_synth )
  g.connect_by_name( synth,             verif_post_synth )
  g.connect( rtl.o('design.v'), verif_post_synth.i('design.ref.v') )
  g.connect( synth.o('design.v'), verif_post_synth.i('design.impl.v') )

  g.connect_by_name( adk,            verif_post_layout )
  g.connect_by_name( synth,             verif_post_layout )
  # g.connect( synth.o('design.v'), vcs_sim.i('design.v') )
  g.connect_by_name( signoff,        vcs_sim      )
  g.connect_by_name( rtl,        vcs_sim      )
  g.connect_by_name( rtl,        vcs_sim_rtl  )
  # g.connect( rtl.o('testbench.sv'), vcs_sim.i('testbench.sv') )
  g.connect( signoff.o('design.lvs.v'), verif_post_layout.i('design.impl.v') )


  #-----------------------------------------------------------------------
  # Parameterize
  #-----------------------------------------------------------------------

  g.update_params( parameters )

  return g


if __name__ == '__main__':
  g = construct()
#  g.plot()

