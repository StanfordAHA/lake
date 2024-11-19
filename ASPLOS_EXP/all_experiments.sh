# # # This sweeps FW without changing num in ports num out ports
# # python ASPLOS_EXP/create_all_experiments.py --fetch_width 2 4 8 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports
# # # Next few commands generate the same things but add in the extra ports
# # python ASPLOS_EXP/create_all_experiments.py --fetch_width 2 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports
# # python ASPLOS_EXP/create_all_experiments.py --fetch_width 4 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 2 --out_ports 2 --storage_capacity 16384 --use_ports
# # python ASPLOS_EXP/create_all_experiments.py --fetch_width 8 --outdir MEK_fw --physical --design_filter single_port_wide_fetch --in_ports 4 --out_ports 4 --storage_capacity 16384 --use_ports
# # python ASPLOS_EXP/create_all_experiments.py --fetch_width 2 --outdir MEK_fwspst --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports --spst dense
# # python ASPLOS_EXP/create_all_experiments.py --fetch_width 2 --outdir MEK_fwspst --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports --spst perf

# # # This sweeps FW without changing num in ports num out ports
# # python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 2 4 8 --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_9_26/ --physical --design_filter single_port_wide_fetch --storage_capacity 16384 --run_builds
# # # Next few commands generate the same things but add in the extra ports
# # python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 2 --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_9_26/ --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 4 --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_9_26/ --physical --design_filter single_port_wide_fetch --in_ports 2 --out_ports 2 --storage_capacity 16384 --use_ports --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 8 --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_9_26/ --physical --design_filter single_port_wide_fetch --in_ports 4 --out_ports 4 --storage_capacity 16384 --use_ports --run_builds

# # # Just create two different builds with the performance and density switch for the SRAM so we can compare (only for the single port!!!!)
# # python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 2 --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_SPST_9_26/ --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports --run_builds --spst dense
# # python ASPLOS_EXP/create_mflowgen_experiments.py --fetch_width 2 --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_SPST_9_26/ --physical --design_filter single_port_wide_fetch --in_ports 1 --out_ports 1 --storage_capacity 16384 --use_ports --run_builds --spst perf

# # just normal simple dual port
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/ASPLOS_SIMPLE_DUAL_PORT_CONFIG_MEM/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds

# # Simple dual port placeholder...
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/SIMPLE_DUAL_PORT_LVS_VLOG_FIX_10_14_SPI/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/SIMPLE_DUAL_PORT_LVS_VLOG_FIX_10_14_RTL_SIM/ --physical --design_filter simple_dual_port --storage_capacity 16384

# # Amber quad static + rv
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/QUAD_PORT_POWER_ALL_PORTS_RV/ --physical --design_filter dual_port_wide_fetch_quad_rv --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/QUAD_PORT_POWER_VLOG_FIX/ --physical --design_filter dual_port_wide_fetch_quad --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/QUAD_PORT_POWER_LONG_TEST/ --physical --design_filter dual_port_wide_fetch_quad --storage_capacity 16384 --run_builds

# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/ASPLOS_SIMPLE_DUAL_PORT_CONFIG_MEM_VLOG_FIX/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/ASPLOS_SIMPLE_DUAL_PORT_CONFIG_MEM_NO_FIX/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/ASPLOS_SIMPLE_DUAL_PORT_CONFIG_MEM_DONTUSE_FIX/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds

# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/ASPLOS_SIMPLE_DUAL_PORT_CONFIG_MEM_VLOG_FIX_LVS/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/ASPLOS_SIMPLE_DUAL_PORT_CONFIG_MEM_NO_FIX_LVS/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # Good cell - SC7P5T_INVX0P5_SSC14R
# # Bad cell - SC7P5T_INVX2_SSC14R

# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_NO_CG_10_23/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_NO_CG_10_23/ --physical --design_filter dual_port_rv --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/QUAD_PORT_NO_CG_10_23/ --physical --design_filter dual_port_wide_fetch_quad --storage_capacity 16384 --run_builds

# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_CG_10_23/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_CG_10_23/ --physical --design_filter dual_port_rv --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/QUAD_PORT_CG_10_23/ --physical --design_filter dual_port_wide_fetch_quad --storage_capacity 16384 --run_builds

# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_CLK_GATE/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_CLK_GATE/ --physical --design_filter dual_port_rv --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/QUAD_PORT_CLK_GATE/ --physical --design_filter dual_port_wide_fetch_quad --storage_capacity 16384 --run_builds

# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/QUAD_PORT_POWER_ALL_PORTS_RV/ --physical --design_filter dual_port_wide_fetch_quad --storage_capacity 16384 --run_builds

# Testing removing the config register entirely...
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_WO_HARD_CFG_REG/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_WO_HARD_CFG_REG/ --physical --design_filter dual_port_rv --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/QUAD_PORT_WO_HARD_CFG_REG/ --physical --design_filter dual_port_wide_fetch_quad --storage_capacity 16384 --run_builds

# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_CONSTRAINT_WITH_CFG_REG/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds
# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/DUAL_PORT_CONSTRAINT_WO_CFG_REG/ --physical --design_filter simple_dual_port --storage_capacity 16384 --run_builds

# # python ASPLOS_EXP/create_mflowgen_experiments.py --build_dir /sim/mstrange/ALL_SWEEP_9_17 --physical --run_builds

# # All figures

# # Summary area + power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment summary --figure_name summary_area
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment summary_power --figure_name summary_power

# # Dual quad summary + power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment dual_quad_summary --figure_name dual_quad_summary_area
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment dual_quad_summary_power --figure_name dual_quad_summary_power

# # Dual quad ports + power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment dual_quad_ports --figure_name dual_quad_ports_area
# # python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment dual_quad_ports_power --figure_name dual_quad_ports_power

# # Config
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment config --figure_name config_area

# # Fetch width sweep iso ports area + power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_9_26/ --csv_out . --experiment fw_sweep_iso_ports --figure_name fw_sweep_iso_ports_area
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_9_26/ --csv_out . --experiment fw_sweep_iso_ports_power --figure_name fw_sweep_iso_ports_power

# # Fetch width sweep grow ports area + power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_9_26/ --csv_out . --experiment fw_sweep_grow_ports --figure_name fw_sweep_grow_ports_area
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_9_26/ --csv_out . --experiment fw_sweep_grow_ports_power --figure_name fw_sweep_grow_ports_power

# # Fetch width sweep grow ports per port area + power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment fw_sweep_grow_ports_per_port --figure_name fw_sweep_grow_ports_per_port_area
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment fw_sweep_grow_ports_per_port_power --figure_name fw_sweep_grow_ports_per_port_power

# # SPST area + power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_SPST_9_26/ --csv_out . --experiment spst --figure_name spst_area
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_FW_SWEEP_SPST_9_26/ --csv_out . --experiment spst_power --figure_name spst_power

# # control v capacity area + power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment control_v_capacity --figure_name control_v_capacity_area
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment control_v_capacity_power --figure_name control_v_capacity_power

# # control v runtime area + power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment control_v_runtime --figure_name control_v_runtime_area
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/ASPLOS_SWEEP_9_23/ --csv_out . --experiment control_v_runtime_power --figure_name control_v_runtime_power

# # Simple dual port, quad port power comparison
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/DUAL_PORT_POWER_CLEAN_10_14/ --csv_out . --experiment dual_port_power --figure_name dual_port_power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/QUAD_PORT_POWER/ --csv_out . --experiment dual_port_power --figure_name quad_port_power
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/QUAD_PORT_POWER_LONG_TEST/ --csv_out . --experiment dual_port_power --figure_name quad_port_power

# # Simple dual port clock gate
# python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/SIMPLE_DUAL_PORT_CLK_GATE/ --csv_out . --experiment dual_port_power --figure_name clkgate_power

# Dual port, quad port w/ clk gate
python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/DUAL_PORT_CLK_GATE/ --csv_out . --experiment dual_port_power --figure_name dpcg
python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/QUAD_PORT_CLK_GATE/ --csv_out . --experiment dual_port_power --figure_name qpcg

# All ports
python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/QUAD_PORT_POWER_ALL_PORTS_RV/ --csv_out . --experiment dual_port_power --figure_name allports_power

cd $LAKE_TOP
python MICRO24_WS/demo_driver.py --outdir LAKE_TEST --verilog --visualize

cd LAKE_TEST
# optionally set WAVEFORM
export WAVEFORM=1
make sim

# Dual port, quad port w/ clk gate
python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/DUAL_PORT_NO_CG_10_23/ --csv_out . --experiment dual_port_power --figure_name dp_no_cg_10_23
python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/DUAL_PORT_CG_10_23/ --csv_out . --experiment dual_port_power --figure_name dp_yes_cg_10_23
python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/QUAD_PORT_NO_CG_10_23/ --csv_out . --experiment dual_port_power --figure_name qp_no_cg_10_23
python ASPLOS_EXP/create_mflowgen_experiments.py --collect_data --build_dir /sim/mstrange/QUAD_PORT_CG_10_23/ --csv_out . --experiment dual_port_power --figure_name qp_yes_cg_10_23