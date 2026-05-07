# Smoke test: 2 known-passing configs + 2 gen_sram fix configs
# Known pass: PORT_EXP fw=4/dw=16 and fw=2/dw=32 (W01024B064)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/SMOKE_BUILDS/PORT_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 4 \
    --vec_capacity 2 \
    --data_width 16 \
    --storage_capacity 8192 \
    --frequency 700

python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/SMOKE_BUILDS/PORT_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 2 \
    --vec_capacity 2 \
    --data_width 32 \
    --storage_capacity 8192 \
    --frequency 700

# gen_sram fix: MEMORY_EXP sc=2048 fw=2 single-port (needed W00512B032, got W01024B064 before)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/SMOKE_BUILDS/MEMORY_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 2 \
    --in_ports 1 \
    --out_ports 1 \
    --data_width 16 \
    --storage_capacity 2048 \
    --frequency 700

# gen_sram fix: MEMORY_EXP sc=4096 fw=1 dual-port (needed W02048B016, got W01024B064 before)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/SMOKE_BUILDS/MEMORY_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 1 \
    --dual_port \
    --in_ports 1 \
    --out_ports 1 \
    --data_width 16 \
    --storage_capacity 4096 \
    --frequency 700
