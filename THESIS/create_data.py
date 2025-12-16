#!/usr/bin/env python3
import argparse
import os
import numpy as np
import matplotlib.pyplot as plt

figure_filenames = [
    'figures/lake_software_flow.pdf',
    'figures/lake_system.pdf',
    'figures/lake_simple_dual_port.pdf',
    'figures/lake_components_address_generator.pdf',
    'figures/lake_components_iteration_domain.pdf',
    'figures/lake_components_address_generator.pdf',
    'figures/lake_components_schedule_generator.pdf',
    'figures/lake_components_address_generator.pdf',
    'figures/lake_components_storage.pdf',
    'figures/lake_components_port_vectorized_write.pdf',
    'figures/lake_components_port_vectorized_read.pdf',
    'figures/lake_components_port_vectorized_write.pdf',
    'figures/lake_components_address_generator_custom.pdf',
    'figures/lake_strides_versus_deltas.pdf',
    'figures/basic_cgra.pdf',
    'figures/basic_cgra_interconnections.pdf',
    'figures/basic_cgra_SB.pdf',
    'figures/basic_cgra_tile.pdf',
    'figures/cgra_software_flow.pdf',
    'figures/port_characterization_interface_width_area.pdf',
    'figures/port_characterization_interface_width_power.pdf',
    'figures/port_characterization_vectorization_area.pdf',
    'figures/port_characterization_vectorization_power.pdf',
    'figures/iteration_domain_characterization_dimensionality_area.pdf',
    'figures/iteration_domain_characterization_dimensionality_power.pdf',
    'figures/iteration_domain_characterization_max_extent_area.pdf',
    'figures/iteration_domain_characterization_max_extent_power.pdf',
    'figures/affine_pattern_generator_characterization_stride_area.pdf',
    'figures/affine_pattern_generator_characterization_stride_power.pdf',
    'figures/affine_pattern_generator_characterization_offset_area.pdf',
    'figures/affine_pattern_generator_characterization_offset_power.pdf',
    'figures/affine_pattern_generator_characterization_dimensionality_area.pdf',
    'figures/affine_pattern_generator_characterization_dimensionality_power.pdf',
    'figures/schedule_generator_characterization_dimensionality.pdf',
    'figures/schedule_generator_characterization_max_extent.pdf',
    'figures/memory_port_characterization_interface_width_area.pdf',
    'figures/memory_port_characterization_interface_width_power.pdf',
    'figures/storage_characterization_capacity_area.pdf',
    'figures/storage_characterization_capacity_power.pdf'
]

figure_filenames_use = [
    # 'lake_software_flow.pdf',
    # 'lake_system.pdf',
    # 'lake_simple_dual_port.pdf',
    # 'lake_components_address_generator.pdf',
    # 'lake_components_iteration_domain.pdf',
    # 'lake_components_address_generator.pdf',
    # 'lake_components_schedule_generator.pdf',
    # 'lake_components_address_generator.pdf',
    # 'lake_components_storage.pdf',
    # 'lake_components_port_vectorized_write.pdf',
    # 'lake_components_port_vectorized_read.pdf',
    # 'lake_components_port_vectorized_write.pdf',
    # 'lake_components_address_generator_custom.pdf',
    # 'lake_strides_versus_deltas.pdf',
    # 'basic_cgra.pdf',
    # 'basic_cgra_interconnections.pdf',
    # 'basic_cgra_SB.pdf',
    # 'basic_cgra_tile.pdf',
    # 'cgra_software_flow.pdf',
    'port_characterization_interface_width_area.pdf',
    'port_characterization_interface_width_power.pdf',
    'port_characterization_vectorization_area.pdf',
    'port_characterization_vectorization_power.pdf',
    'iteration_domain_characterization_dimensionality_area.pdf',
    'iteration_domain_characterization_dimensionality_power.pdf',
    'iteration_domain_characterization_max_extent_area.pdf',
    'iteration_domain_characterization_max_extent_power.pdf',
    'affine_pattern_generator_characterization_stride_area.pdf',
    'affine_pattern_generator_characterization_stride_power.pdf',
    'affine_pattern_generator_characterization_offset_area.pdf',
    'affine_pattern_generator_characterization_offset_power.pdf',
    'affine_pattern_generator_characterization_dimensionality_area.pdf',
    'affine_pattern_generator_characterization_dimensionality_power.pdf',
    'schedule_generator_characterization_dimensionality.pdf',
    'schedule_generator_characterization_max_extent.pdf',
    'memory_port_characterization_interface_width_area.pdf',
    'memory_port_characterization_interface_width_power.pdf',
    'storage_characterization_capacity_area.pdf',
    'storage_characterization_capacity_power.pdf'
]


def plot_clustered_4():

    num_apps = 10
    num_designs = 4

    applications = [f"App{i+1}" for i in range(num_apps)]

    # Fake cost data (replace with your own)
    np.random.seed(0)
    costs = np.random.randint(10, 100, size=(num_designs, num_apps))

    design_labels = [f"Design {i+1}" for i in range(num_designs)]

    # ----- Plotting -----
    x = np.arange(num_apps)  # application positions
    bar_width = 0.2

    fig, ax = plt.subplots(figsize=(14, 6))

    for i in range(num_designs):
        ax.bar(x + i * bar_width, costs[i], width=bar_width, label=design_labels[i])

    ax.set_xlabel("Applications")
    ax.set_ylabel("Total Cost")
    ax.set_title("Total Cost of Applications Across Designs")
    ax.set_xticks(x + bar_width * (num_designs - 1) / 2)
    ax.set_xticklabels(applications, rotation=45, ha="right")

    ax.legend()
    ax.grid(axis="y", linestyle="--", alpha=0.6)

    plt.tight_layout()
    # plt.show()

    single_level_filenames = [
        'single_level_area.pdf',
        'single_level_power.pdf',
        'single_level_performance.pdf',
        'single_level_utilization.pdf',
        'single_level_energy_efficiency.pdf',
        'two_level_area.pdf',
        'two_level_power.pdf',
        'two_level_performance.pdf',
        'two_level_utilization.pdf',
        'two_level_energy_efficiency.pdf',
        'latency_insensitive_schedule_generator_characterization_ports_power.pdf',
        'latency_insensitive_schedule_generator_characterization_ports_area.pdf',
        'latency_insensitive_schedule_generator_characterization_loc_power.pdf',
        'latency_insensitive_schedule_generator_characterization_loc_area.pdf',
    ]

    print("Printing single-level designs!")

    for f_ in single_level_filenames:
        print(f"✅ Figure used: {f_}")
        plt.savefig(os.path.join("output", f_))


def main():
    # Argument parser setup
    parser = argparse.ArgumentParser(
        description="Generate a stacked bar chart of synthesized area vs. dimensionality."
    )
    # parser.add_argument(
    #     "output_pdf",
    #     type=str,
    #     help="Path to output PDF file (e.g., output.pdf)"
    # )
    parser.add_argument(
        "--single",
        action="store_true",
        help="Enable the feature (default: disabled)."
    )

    args = parser.parse_args()

    if args.single:
        plot_clustered_4()
        return

    # output_pdf = args.output_pdf

    # Bogus data
    dimensionality = np.array([2, 4, 8, 16, 32])
    combinational = np.array([100, 180, 260, 350, 480])
    sequential = np.array([60, 110, 160, 230, 310])
    config_regs = np.array([40, 80, 130, 180, 240])

    # Create the stacked bar chart
    bar_width = 0.6
    fig, ax = plt.subplots(figsize=(8, 5))

    ax.bar(dimensionality, combinational, width=bar_width, label='Combinational Logic')
    ax.bar(dimensionality, sequential, width=bar_width, bottom=combinational, label='Sequential Logic')
    ax.bar(dimensionality, config_regs, width=bar_width,
           bottom=combinational + sequential, label='Configuration Registers')

    # Labels and style
    ax.set_xlabel('Dimensionality', fontsize=12)
    ax.set_ylabel('Synthesized Area (arbitrary units)', fontsize=12)
    ax.set_title('Synthesized Area Breakdown by Logic Type vs. Dimensionality', fontsize=14)
    ax.legend()
    ax.grid(axis='y', linestyle='--', alpha=0.6)

    plt.tight_layout()
    for f_ in figure_filenames_use:
        print(f"✅ Figure used: {f_}")
        plt.savefig(os.path.join("output", f_))
    # print(f"✅ Plot saved to {output_pdf}")

if __name__ == "__main__":
    main()