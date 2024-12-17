set target_module [get_cells port_ag_0]
set AGINPINS [get_pins -of_objects $target_module -filter "direction==in"]
report_timing -through $AGINPINS -nworst 10 > ~/ag_path_through.txt
