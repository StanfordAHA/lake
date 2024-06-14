#=========================================================================
# pt_px.tcl
#=========================================================================
# We use Synopsys PrimeTime to get the power analysis of the PnR netlist.
#
# This PrimeTime power analysis step follows the signoff step to estimate
# the average power of the gate-level netlist using RTL SAIF file.
#
# Author : Shady Agwa, Yanghui Ou
# Date   : May 7, 2019
#

#-------------------------------------------------------------------------
# Interface to the ASIC design kit
#-------------------------------------------------------------------------

set ptpx_additional_search_path   inputs/adk
set ptpx_target_libraries         inputs/adk/stdcells.db
set ptpx_extra_link_libraries     [glob -nocomplain inputs/*.db]

#-------------------------------------------------------------------------
# Interface to the build system
#-------------------------------------------------------------------------

set ptpx_design_name              $::env(design_name)

set ptpx_gl_netlist               [glob -nocomplain inputs/*.vcs.v]
set ptpx_sdc                      [glob -nocomplain inputs/*.pt.sdc]
set ptpx_spef                     [glob -nocomplain inputs/*.spef.gz]

#-------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------

# Set up paths and libraries

set_app_var search_path      ". $ptpx_additional_search_path $search_path"
set_app_var target_library   $ptpx_target_libraries
set_app_var link_library     "* $ptpx_target_libraries $ptpx_extra_link_libraries"
set_app_var power_enable_analysis true

#-------------------------------------------------------------------------
# Read design
#-------------------------------------------------------------------------

# Read and link the design

read_verilog   $ptpx_gl_netlist
current_design $ptpx_design_name

# Read in the SDC and parasitics

read_sdc -echo $ptpx_sdc
read_parasitics -format spef $ptpx_spef

set pt_reports reports
set pt_clk clk 
set pt_uut $::env(strip_path)
set pt_clk_period $::env(clock_period)

file mkdir ${pt_reports}
create_clock ${pt_clk} -name ideal_clock1 -period ${pt_clk_period}
source inputs/design.namemap > ${pt_reports}/${ptpx_design_name}.map.rpt

read_saif inputs/run.saif -strip_path ${pt_uut}

update_power > ${pt_reports}/${ptpx_design_name}.update.rpt
report_switching_activity > ${pt_reports}/${ptpx_design_name}.sw.rpt 
report_power -nosplit  -verbose > ${pt_reports}/${ptpx_design_name}.pwr.rpt
report_power -nosplit -hierarchy -verbose > ${pt_reports}/${ptpx_design_name}.pwr.hier.rpt
exit
