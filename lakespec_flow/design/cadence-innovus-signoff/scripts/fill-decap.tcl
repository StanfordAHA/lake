# Replace some fillers with decaps to meet density requirements

# Set the desired core VDD/VSS decap value for the entire design
set core_decap_value_pF 1000
# Capacitance were values obtained in simulation from RC -3dB corner (commands expect fF)

set decap_cells [list sky130_fd_sc_hd__decap_12 \
                      sky130_fd_sc_hd__decap_8 \
                      sky130_fd_sc_hd__decap_6 \
                      sky130_fd_sc_hd__decap_4 \
                      sky130_fd_sc_hd__decap_3 \
                ]
# these values may not be correct for sky130, but it is okay
set decap_values_fF [list 531.681 255.288 128.376 56.747 11.216]
# Populate decap candidates
foreach capname $decap_cells capvalue $decap_values_fF {
    addDeCapCellCandidates $capname $capvalue
}
# Add decap in between filler (requires earlier call to setFillerMode)
deleteFiller
addDeCap -totCap [expr $core_decap_value_pF * 1000] -cells $decap_cells -prefix DECAP
addFiller
# ecoRoute -target