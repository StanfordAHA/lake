# 7.2.1 Streaming Memory Characteristics
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Experiments to sweep over port characteristics...
# NOTE: fw=4 dw=64 (depth=256, width=256) has no valid GF single-port macro (min depth 512 for width 256)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/PORT_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 4 \
    --in_ports 2 \
    --out_ports 2 \
    --data_width 8 16 32 \
    --storage_capacity 8192 \
    --frequency 700

# NOTE: No valid GF macro for: fw=8 dw=64 (depth=128, width=512), fw=8 dw=32 (depth=256, width=256),
#   fw=4 dw=64 (depth=256, width=256). All have depth too small for their width class.
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/PORT_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 2 4 8 \
    --vec_capacity 2 4 8 \
    --data_width 8 16 \
    --storage_capacity 8192 \
    --frequency 700

python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/PORT_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 2 4 \
    --vec_capacity 2 4 8 \
    --data_width 32 \
    --storage_capacity 8192 \
    --frequency 700

python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/PORT_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 2 \
    --vec_capacity 2 4 8 \
    --data_width 64 \
    --storage_capacity 8192 \
    --frequency 700

# Experiments to sweep over IterationDomain characteristics...dimensionality and max extent
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/ITERATION_DOMAIN_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 1 \
    --data_width 16 \
    --dimensionality 1 2 3 4 5 6 \
    --max_extent 64 256 1024 4096 \
    --storage_capacity 8192 \
    --frequency 700

# Experiments to sweep over AddressGenerator characteristics...dimensionality, max offset/stride are set by sequence value width
# Strides could be minimized (if we only expect to step by a certain amount on the inside...)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/AFFINE_PATTERN_GENERATOR_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 1 \
    --data_width 16 \
    --dimensionality 1 2 3 4 5 6 \
    --max_sequence_width 64 256 1024 4096 16384 \
    --storage_capacity 8192 \
    --frequency 700

# Experiments to sweep over Storage (capacity) and MemoryPort (type, shared ports) characteristics...capacity and line size

# Single fetch dual port, 1 in 1 out
# NOTE: sc=32768 (depth=16384, width=16) has no valid GF dual-port macro
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/MEMORY_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 1 \
    --dual_port \
    --in_ports 1 \
    --out_ports 1 \
    --data_width 16 \
    --storage_capacity 1024 2048 4096 8192 16384 \
    --frequency 700

# 2 fetch dual port, 2 in 2 out
# NOTE: sc=32768 (depth=8192, width=32) has no valid GF dual-port macro
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/MEMORY_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 2 \
    --dual_port \
    --in_ports 2 \
    --out_ports 2 \
    --data_width 16 \
    --storage_capacity 1024 2048 4096 8192 16384 \
    --frequency 700

# 4 fetch dual port, 4 in 4 out
# NOTE: sc=16384,32768 (depth>=4096, width=64) have no valid GF dual-port macro
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/MEMORY_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 4 \
    --dual_port \
    --in_ports 4 \
    --out_ports 4 \
    --data_width 16 \
    --storage_capacity 1024 2048 4096 8192 \
    --frequency 700

# 8 fetch dual port, 8 in 8 out
# NOTE: All capacities at fw=8 dual-port (width=128) have no valid GF dual-port macro
# python ASPLOS_EXP/create_mflowgen_experiments.py \
#     --build_dir /sim/mstrange/THESIS_BUILDS/MEMORY_EXP \
#     --design_filter thesis_sweep --physical --run_builds \
#     --fetch_width 8 \
#     --dual_port \
#     --in_ports 8 \
#     --out_ports 8 \
#     --data_width 16 \
#     --storage_capacity 1024 2048 4096 8192 16384 32768 \
#     --frequency 700

# Dual fetch single port, 1 in 1 out
# NOTE: sc=1024 (depth=256, width=32) has no valid GF single-port macro (min depth 512)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/MEMORY_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 2 \
    --in_ports 1 \
    --out_ports 1 \
    --data_width 16 \
    --storage_capacity 2048 4096 8192 16384 32768 \
    --frequency 700

# Quad fetch single port, 2 in 2 out
# NOTE: sc=1024,2048 (depth=128/256, width=64) have no valid GF single-port macro (min depth 512)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/MEMORY_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 4 \
    --in_ports 2 \
    --out_ports 2 \
    --data_width 16 \
    --storage_capacity 4096 8192 16384 32768 \
    --frequency 700

# 8 fetch single port, 4 in 4 out
# NOTE: sc=1024,2048,4096 (depth=64/128/256, width=128) have no valid GF single-port macro (min depth 512)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/MEMORY_EXP \
    --design_filter thesis_sweep --physical --run_builds \
    --fetch_width 8 \
    --in_ports 4 \
    --out_ports 4 \
    --data_width 16 \
    --storage_capacity 8192 16384 32768 \
    --frequency 700

# Unified Buffer Characterization
# This section is more focused on the performance of the memory system as a whole
# May actually already have all the data we need from prior experiments
# Could do a few more focused experiments here if needed
