#! /usr/bin/env python
#=========================================================================
# construct.py
#=========================================================================
# Flow specification for HLS design
#
# Author : Priyanka Raina & Marc Huerta
#

import os
import sys

from mflowgen.components import Graph, Step

def construct():

  g = Graph()
  g.sys_path.append( '/sim/mah2001' )

  #-----------------------------------------------------------------------
  # Parameters
  #-----------------------------------------------------------------------
  
  adk_name = 'skywater-130nm-adk'
  adk_view = 'view-standard'

  parameters = {
    'construct_path' : __file__,
    'design_name'    : 'lakespec',
    'clock_period'   : 100.0,
    'adk'            : adk_name,
    'adk_view'       : adk_view,
    'topographical'  : False,
    # 'testbench_name' : 'Conv_tb',
    # 'strip_path'     : 'scverify_top/Pathtracer',
    # 'saif_instance'  : 'scverify_top/Pathtracer'
  }

  #-----------------------------------------------------------------------
  # Create nodes
  #-----------------------------------------------------------------------

  this_dir = os.path.dirname( os.path.abspath( __file__ ) )

  # ADK step

  g.set_adk( adk_name )
  adk = g.get_adk_step()

  # Custom steps

  # sram          = Step( this_dir + '/sram'          )
  rtl           = Step( this_dir + '/rtl'           )
  # hls           = Step( this_dir + '/hls'           )
  # testbench     = Step( this_dir + '/testbench'     )
  constraints   = Step( this_dir + '/constraints'   )
  
  signoff_area_plotting  = Step( this_dir + '/signoff_area_plotting'           )
  power_pt_gl_plotting  = Step( this_dir + '/power_pt_gl_plotting'           )


  # Power node is custom because power and gnd pins are named differently in
  # the standard cells compared to the default node, and the layer numbering is
  # different because of li layer, the default assumes metal 1 is the lowest
  # layer
  
  power           = Step( this_dir + '/cadence-innovus-power'           ) 
  floorplan       = Step( this_dir + '/floorplan'                       )
  dont_use_cells       = Step( this_dir + '/cadence-innovus-dont-use-cells' )
  pin_placement   = Step( this_dir + '/pin-placement'                   )

  # Signoff is custom because it has to output def that the default step does
  # not do. This is because we use the def instead of gds for generating spice
  # from layout for LVS
  
  signoff         = Step( this_dir + '/cadence-innovus-signoff'         ) 
  
  pt_power_rtl    = Step( this_dir + '/synopsys-ptpx-rtl'               )

  magic_drc       = Step( this_dir + '/open-magic-drc'                  )
  magic_def2spice = Step( this_dir + '/open-magic-def2spice'            )
  magic_gds2spice = Step( this_dir + '/open-magic-gds2spice'            )
  netgen_lvs      = Step( this_dir + '/open-netgen-lvs'                 )
  magic_antenna   = Step( this_dir + '/open-magic-antenna'              )
  calibre_lvs     = Step( this_dir + '/mentor-calibre-comparison'       )
  pt_power_gl     = Step( this_dir + '/synopsys-ptpx-gl'       )




  # Default steps

  info         = Step( 'info',                          default=True )
  dc           = Step( 'synopsys-dc-synthesis',         default=True )
  # rtl_sim      = Step( 'synopsys-vcs-sim',              default=True )
  # gen_saif     = Step( 'synopsys-vcd2saif-convert',     default=True )
  # gen_saif_rtl = gen_saif.clone()
  # gen_saif_rtl.set_name( 'gen-saif-rtl' )



  iflow           = Step( 'cadence-innovus-flowsetup',     default=True )
  init            = Step( 'cadence-innovus-init',          default=True )
  place           = Step( 'cadence-innovus-place',         default=True )
  cts             = Step( 'cadence-innovus-cts',           default=True )
  postcts_hold    = Step( 'cadence-innovus-postcts_hold',  default=True )
  route           = Step( 'cadence-innovus-route',         default=True )
  postroute       = Step( 'cadence-innovus-postroute',     default=True )
  postroute_hold  = Step( 'cadence-innovus-postroute_hold',     default=True )
  gdsmerge        = Step( 'mentor-calibre-gdsmerge',       default=True )
  pt_timing       = Step( 'synopsys-pt-timing-signoff',    default=True )

  netgen_lvs_def  = netgen_lvs.clone()
  netgen_lvs_def.set_name('netgen-lvs-def')
  netgen_lvs_gds  = netgen_lvs.clone()
  netgen_lvs_gds.set_name('netgen-lvs-gds')

  # pt_power_gl     = Step( 'synopsys-ptpx-gl',              default=True )

  #-----------------------------------------------------------------------
  # Graph -- Add nodes
  #-----------------------------------------------------------------------

  g.add_step( info         )
  # g.add_step( sram         )
  g.add_step( rtl          )
  # g.add_step( hls          )
  g.add_step( power_pt_gl_plotting )
  g.add_step( signoff_area_plotting          )
  # g.add_step( testbench    )
  g.add_step( constraints  )
  g.add_step( dc           )
  # g.add_step( rtl_sim      )
  # g.add_step( gen_saif_rtl )
  g.add_step( iflow           )
  g.add_step( init            )
  g.add_step( power           )
  g.add_step( dont_use_cells  ) 
  g.add_step( floorplan       )
  g.add_step( pin_placement    )
  g.add_step( place           )
  g.add_step( cts             )
  g.add_step( postcts_hold    )
  g.add_step( route           )
  g.add_step( postroute       )
  g.add_step( postroute_hold  )
  g.add_step( signoff         )
  g.add_step( gdsmerge        )
  g.add_step( pt_timing       )
  g.add_step( pt_power_rtl    )
  g.add_step( pt_power_gl     )
  g.add_step( magic_drc       )
  g.add_step( magic_antenna   )
  g.add_step( magic_def2spice )
  g.add_step( netgen_lvs_def  )
  g.add_step( magic_gds2spice )
  g.add_step( netgen_lvs_gds  )
  g.add_step( calibre_lvs     )
  #-----------------------------------------------------------------------
  # Graph -- Add edges
  #-----------------------------------------------------------------------
  
  # Dynamically add edges

  # dc.extend_inputs(['sram1_tt_1p8V_25C.db'])
  # dc.extend_inputs(['sram2_tt_1p8V_25C.db'])
  # rtl_sim.extend_inputs(['sram.v'])
  init.extend_inputs(['dont-use-cells.tcl', 'floorplan.tcl', 'pin-assignments.tcl'])

  # Connect by name
  g.connect_by_name( adk,          dc           )
  # g.connect_by_name( sram,         dc           )
  # g.connect_by_name( hls,          dc           )
  g.connect_by_name( rtl,          dc           )
  g.connect_by_name( constraints,  dc           )
  # g.connect_by_name( hls,          rtl_sim      ) 
  # g.connect_by_name( testbench,    rtl_sim      ) 
  # g.connect_by_name( sram,         rtl_sim      ) 
  # g.connect( rtl_sim.o( 'run.vcd' ), gen_saif_rtl.i( 'run.vcd' ) ) # FIXME: VCS sim node generates a VCD file but gives it a VPD extension
  # g.connect( hls.o( 'run.vcd' ), gen_saif_rtl.i( 'run.vcd' ) )
  # g.connect_by_name( gen_saif_rtl, dc           ) # run.saifs

  g.connect_by_name( adk,             iflow           )
  g.connect_by_name( adk,             init            )
  g.connect_by_name( adk,             power           )
  g.connect_by_name( adk,             place           )
  g.connect_by_name( adk,             cts             )
  g.connect_by_name( adk,             postcts_hold    )
  g.connect_by_name( adk,             route           )
  g.connect_by_name( adk,             postroute       )
  g.connect_by_name( adk,             postroute_hold  )  
  g.connect_by_name( adk,             signoff         )
  g.connect_by_name( adk,             gdsmerge        )
  g.connect_by_name( adk,             magic_drc       )
  g.connect_by_name( adk,             magic_antenna   )
  g.connect_by_name( adk,             magic_def2spice )
  g.connect_by_name( adk,             magic_gds2spice )
  g.connect_by_name( adk,             netgen_lvs_def  )
  g.connect_by_name( adk,             netgen_lvs_gds  )
  g.connect_by_name( adk,             calibre_lvs     )
  g.connect_by_name( adk,             pt_timing       )
  g.connect_by_name( adk,             pt_power_rtl    )
  g.connect_by_name( adk,             pt_power_gl     )

  g.connect_by_name( dc,              iflow           )
  g.connect_by_name( dc,              init            )
  g.connect_by_name( dc,              power           )
  g.connect_by_name( dc,              place           )
  g.connect_by_name( dc,              cts             )
  g.connect_by_name( dc,              pt_power_rtl    ) # design.namemap

  g.connect_by_name( iflow,           init            )
  g.connect_by_name( iflow,           power           )
  g.connect_by_name( iflow,           place           )
  g.connect_by_name( iflow,           cts             )
  g.connect_by_name( iflow,           postcts_hold    )
  g.connect_by_name( iflow,           route           )
  g.connect_by_name( iflow,           postroute       )
  g.connect_by_name( iflow,           postroute_hold  )
  g.connect_by_name( iflow,           signoff         )
  
  # Core place and route flow
  g.connect_by_name( init,            power           )
  g.connect_by_name( power,           place           )
  g.connect_by_name( place,           cts             )
  g.connect_by_name( cts,             postcts_hold    )
  g.connect_by_name( postcts_hold,    route           )
  g.connect_by_name( route,           postroute       )
  g.connect_by_name( postroute,       postroute_hold  )
  g.connect_by_name( postroute_hold,  signoff         )
  g.connect_by_name( signoff,         signoff_area_plotting)
  g.connect_by_name( signoff,         gdsmerge        )

  
  #############
  g.connect_by_name( floorplan,     init         )
  g.connect_by_name( dont_use_cells,     init    )
  g.connect_by_name( pin_placement, init         )

  # DRC, LVS, timing signoff and power signoff
  g.connect_by_name( gdsmerge,        magic_drc       )
  g.connect_by_name( gdsmerge,        magic_antenna   )

  # LVS using DEF
  g.connect_by_name( signoff,         magic_def2spice )
  g.connect_by_name( signoff,         netgen_lvs_def  )
  g.connect_by_name( magic_def2spice, netgen_lvs_def  )

  # LVS using GDS
  g.connect_by_name( gdsmerge,        magic_gds2spice )
  g.connect_by_name( signoff,         netgen_lvs_gds  )
  g.connect_by_name( magic_gds2spice, netgen_lvs_gds  )

  # LVS comparision using Calibre
  g.connect_by_name( signoff,         calibre_lvs     )
  g.connect_by_name( magic_gds2spice, calibre_lvs     )

  g.connect_by_name( signoff,         pt_timing       )
  g.connect_by_name( signoff,         pt_power_rtl    )
  # g.connect_by_name( gen_saif_rtl,    pt_power_rtl    ) # run.saif
  g.connect_by_name( signoff,         pt_power_gl     )


  g.connect_by_name( pt_power_gl,         power_pt_gl_plotting)

  #-----------------------------------------------------------------------
  # Parameterize
  #-----------------------------------------------------------------------

  g.update_params( parameters )
  g.param_space( 'synopsys-dc-synthesis', 'clock_period', [100] )

  return g

if __name__ == '__main__':
  g = construct()
  g.plot()