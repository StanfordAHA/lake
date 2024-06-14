# Copied from https://github.com/efabless/caravel/blob/master/gds/antenna_on_gds.tcl

gds read inputs/design_merged.gds

load $::env(design_name)

select top cell
extract do local
extract no capacitance
extract no coupling
extract no resisitance
extract no adjust
extract unique
extract

feedback save ./outputs/antenna.feedback.txt

antennacheck debug
antennacheck

quit
