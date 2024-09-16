# This sweeps FW without changing num in ports num out ports
python ASPLOS_EXP/create_all_experiments.py --fetch_width 2 4 8 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports
# Next few commands generate the same things but add in the extra ports
python ASPLOS_EXP/create_all_experiments.py --fetch_width 2 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports
python ASPLOS_EXP/create_all_experiments.py --fetch_width 4 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 2 --out_ports 2 --storage_capacity 16384 --use_ports
python ASPLOS_EXP/create_all_experiments.py --fetch_width 8 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 4 --out_ports 4 --storage_capacity 16384 --use_ports

# This sweeps FW without changing num in ports num out ports
python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 2 4 8 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports
# Next few commands generate the same things but add in the extra ports
python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 2 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports
python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 4 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 2 --out_ports 2 --storage_capacity 16384 --use_ports
python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 8 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 4 --out_ports 4 --storage_capacity 16384 --use_ports