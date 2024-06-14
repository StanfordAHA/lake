#=========================================================================
# main.tcl
#=========================================================================
# Run the foundation flow step
#
# Author : Christopher Torng
# Date   : January 13, 2020

set_interactive_constraint_modes [all_constraint_modes]
source inputs/design.checkpoint/save.enc.dat/mmmc/modes/constraints_default/constraints_default.sdc
source -verbose innovus-foundation-flow/INNOVUS/run_signoff.tcl


