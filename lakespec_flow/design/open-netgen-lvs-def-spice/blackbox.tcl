# Set up SRAM instances as black-box cells
model {sky130_sram_1kbyte_1rw1r_32x256_8 inputs/design_extracted.spice} blackbox
model {sky130_sram_1kbyte_1rw1r_32x256_8 design_lvs.spice} blackbox
