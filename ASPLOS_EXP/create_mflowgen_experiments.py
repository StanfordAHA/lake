import os
import subprocess
import argparse
import time
import json
from lake.utils.util import get_file_contents, check_file_exists_and_has_content
import re
import matplotlib
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

matplotlib.rc('xtick', labelsize=20)
matplotlib.rc('ytick', labelsize=20)

def create_spst_power(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other_total'] = df['MemintfDec_total'] + df['MemoryPort_total']
    print(df)
    df = df[(df.input_ports != 1) | ((df.input_ports == 1) & (df.fw == 2))]
    df = df.drop_duplicates()

    all_categories = [
        'Storage_total',
        'ID_total',
        'AG_total',
        'Config_total',
        'SG_total',
        'Port_total',
        'Other_total',
    ]

    fig, ax = plt.subplots(figsize=(20, 25))

    x = np.arange(len(df['design']))

    bottom = np.zeros(len(df['design']))
    # Stacking the bars
    for category in all_categories:
        p = ax.bar(x, df[category], width=0.5, label=category, bottom=bottom)
        bottom += df[category]

    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Power', fontsize=24)
    ax.set_title('FW/2 input FW/2 output single memory port FW sweep', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(df['fw'], rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_spst(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other'] = df['MemintfDec'] + df['MemoryPort']
    df = df[(df.input_ports != 1) | ((df.input_ports == 1) & (df.fw == 2))]
    df = df.drop_duplicates()

    all_categories = [
        'Storage',
        'ID',
        'AG',
        'Config',
        'SG',
        'Port',
        'Other',
    ]

    fig, ax = plt.subplots(figsize=(20, 25))

    x = np.arange(len(df['design']))

    bottom = np.zeros(len(df['design']))
    # Stacking the bars
    for category in all_categories:
        p = ax.bar(x, df[category], width=0.5, label=category, bottom=bottom)
        bottom += df[category]

    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Areas', fontsize=24)
    ax.set_title('FW/2 input FW/2 output single memory port FW sweep', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(df['fw'], rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_control_v_runtime_power(data, filename="grouped_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)

    # get both of these designs.
    df['all_total'] = df['AG_total'] + df['SG_total'] + df['ID_total'] + df['Port_total'] + df['Config_total']
    df = df[(df.design == 'simple_dual_port') | (df.design == 'dual_port_rv')]
    df = df.sort_values('capacity')
    df['Other_total'] = df['MemintfDec_total'] + df['MemoryPort_total']

    df_static = df[(df.design == 'simple_dual_port')]
    df_rv = df[(df.design == 'dual_port_rv')]

    all_categories = [
        'AG_total',
        'SG_total',
        'ID_total',
        'Port_total',
        'Config_total',
    ]

    fig, ax = plt.subplots(1, 2, figsize=(20, 25))

    x = np.arange(len(df_static['design']))
    width = 0.2

    total_ylim = 0.00012

    # Do it for the static data
    multiplier = 0
    # Grouped
    for category in all_categories:
        offset = width * multiplier
        rects = ax[0].bar(x + offset, df_static[category], width, label=category)
        ax[0].bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax[0].set_xlabel('Capacity', fontsize=24)
    ax[0].set_ylabel('Power', fontsize=24)
    ax[0].set_title('Control v Capacity (static)', fontsize=36)
    ax[0].set_xticks(x + width, df_static['capacity'])
    ax[0].set_xticklabels(df_static['capacity'], rotation=45, fontsize=20)
    # ax[0].set_ylim([0, total_ylim])

    multiplier = 0
    # Grouped
    for category in all_categories:
        offset = width * multiplier
        rects = ax[1].bar(x + offset, df_rv[category], width, label=category)
        ax[1].bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax[1].set_xlabel('Capacity', fontsize=24)
    ax[1].set_ylabel('Power', fontsize=24)
    ax[1].set_title('Control v Capacity (rv)', fontsize=36)
    ax[1].set_xticks(x + width, df_rv['capacity'])
    ax[1].set_xticklabels(df_rv['capacity'], rotation=45, fontsize=20)
    # ax[1].set_ylim([0, total_ylim])

    plt.yticks(fontsize=20)
    # Adding legend
    plt.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_control_v_runtime(data, filename="grouped_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)

    # get both of these designs.
    df['all'] = df['AG'] + df['SG'] + df['ID'] + df['Port'] + df['Config']
    df = df[(df.design == 'simple_dual_port') | (df.design == 'dual_port_rv')]
    df = df.sort_values('capacity')
    df['Other'] = df['MemintfDec'] + df['MemoryPort']

    df_static = df[(df.design == 'simple_dual_port')]
    df_rv = df[(df.design == 'dual_port_rv')]

    all_categories = [
        'AG',
        'SG',
        'ID',
        'Port',
        'Config',
    ]

    fig, ax = plt.subplots(1, 2, figsize=(20, 25))

    x = np.arange(len(df_static['design']))
    width = 0.2

    total_ylim = 600

    # Do it for the static data
    multiplier = 0
    # Grouped
    for category in all_categories:
        offset = width * multiplier
        rects = ax[0].bar(x + offset, df_static[category], width, label=category)
        ax[0].bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax[0].set_xlabel('Capacity', fontsize=24)
    ax[0].set_ylabel('Areas', fontsize=24)
    ax[0].set_title('Control v Capacity (static)', fontsize=36)
    ax[0].set_xticks(x + width, df_static['capacity'])
    ax[0].set_xticklabels(df_static['capacity'], rotation=45, fontsize=20)
    ax[0].set_ylim([0, total_ylim])

    multiplier = 0
    # Grouped
    for category in all_categories:
        offset = width * multiplier
        rects = ax[1].bar(x + offset, df_rv[category], width, label=category)
        ax[1].bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax[1].set_xlabel('Capacity', fontsize=24)
    ax[1].set_ylabel('Areas', fontsize=24)
    ax[1].set_title('Control v Capacity (rv)', fontsize=36)
    ax[1].set_xticks(x + width, df_rv['capacity'])
    ax[1].set_xticklabels(df_rv['capacity'], rotation=45, fontsize=20)
    ax[1].set_ylim([0, total_ylim])

    plt.yticks(fontsize=20)
    # Adding legend
    plt.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_control_v_capacity_power(data, filename="grouped_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['all_total'] = df['AG_total'] + df['SG_total'] + df['ID_total'] + df['Port_total'] + df['Config_total']
    # get both of these designs.
    df = df[(df.design == 'dual_port_wide_fetch_quad') | (df.design == 'dual_port_wide_fetch_quad_rv')]
    df = df.sort_values('capacity')
    df['Other_total'] = df['MemintfDec_total'] + df['MemoryPort_total']

    df_static = df[(df.design == 'dual_port_wide_fetch_quad')]
    df_rv = df[(df.design == 'dual_port_wide_fetch_quad_rv')]

    all_categories = [
        'AG_total',
        'SG_total',
        'ID_total',
        'Port_total',
        'Config_total',
    ]

    fig, ax = plt.subplots(1, 2, figsize=(20, 25))

    x = np.arange(len(df_static['design']))
    width = 0.2

    total_ylim = 0.0005

    # Do it for the static data
    multiplier = 0
    # Grouped
    for category in all_categories:
        offset = width * multiplier
        rects = ax[0].bar(x + offset, df_static[category], width, label=category)
        ax[0].bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax[0].set_xlabel('Capacity', fontsize=24)
    ax[0].set_ylabel('Power', fontsize=24)
    ax[0].set_title('Control v Capacity (static)', fontsize=36)
    ax[0].set_xticks(x + width, df_static['capacity'])
    ax[0].set_xticklabels(df_static['capacity'], rotation=45, fontsize=20)
    # ax[0].set_ylim([0, total_ylim])

    multiplier = 0
    # Grouped
    for category in all_categories:
        offset = width * multiplier
        rects = ax[1].bar(x + offset, df_rv[category], width, label=category)
        ax[1].bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax[1].set_xlabel('Capacity', fontsize=24)
    ax[1].set_ylabel('Power', fontsize=24)
    ax[1].set_title('Control v Capacity (rv)', fontsize=36)
    ax[1].set_xticks(x + width, df_rv['capacity'])
    ax[1].set_xticklabels(df_rv['capacity'], rotation=45, fontsize=20)
    # ax[1].set_ylim([0, total_ylim])

    plt.yticks(fontsize=20)
    # Adding legend
    plt.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_control_v_capacity(data, filename="grouped_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['all'] = df['AG'] + df['SG'] + df['ID'] + df['Port'] + df['Config']
    # get both of these designs.
    df = df[(df.design == 'dual_port_wide_fetch_quad') | (df.design == 'dual_port_wide_fetch_quad_rv')]
    df = df.sort_values('capacity')
    df['Other'] = df['MemintfDec'] + df['MemoryPort']

    df_static = df[(df.design == 'dual_port_wide_fetch_quad')]
    df_rv = df[(df.design == 'dual_port_wide_fetch_quad_rv')]

    all_categories = [
        'AG',
        'SG',
        'ID',
        'Port',
        'Config',
    ]

    fig, ax = plt.subplots(1, 2, figsize=(20, 25))

    x = np.arange(len(df_static['design']))
    width = 0.2

    total_ylim = 4000

    # Do it for the static data
    multiplier = 0
    # Grouped
    for category in all_categories:
        offset = width * multiplier
        rects = ax[0].bar(x + offset, df_static[category], width, label=category)
        ax[0].bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax[0].set_xlabel('Capacity', fontsize=24)
    ax[0].set_ylabel('Areas', fontsize=24)
    ax[0].set_title('Control v Capacity (static)', fontsize=36)
    ax[0].set_xticks(x + width, df_static['capacity'])
    ax[0].set_xticklabels(df_static['capacity'], rotation=45, fontsize=20)
    ax[0].set_ylim([0, total_ylim])

    multiplier = 0
    # Grouped
    for category in all_categories:
        offset = width * multiplier
        rects = ax[1].bar(x + offset, df_rv[category], width, label=category)
        ax[1].bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax[1].set_xlabel('Capacity', fontsize=24)
    ax[1].set_ylabel('Areas', fontsize=24)
    ax[1].set_title('Control v Capacity (rv)', fontsize=36)
    ax[1].set_xticks(x + width, df_rv['capacity'])
    ax[1].set_xticklabels(df_rv['capacity'], rotation=45, fontsize=20)
    ax[1].set_ylim([0, total_ylim])

    plt.yticks(fontsize=20)
    # Adding legend
    plt.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_fw_sweep_grow_ports_per_port_power(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other_total'] = df['MemintfDec_total'] + df['MemoryPort_total']
    df['logic_total'] = df['AG_total'] + df['SG_total'] + df['ID_total']
    df['all_total'] = df['AG_total'] + df['SG_total'] + df['ID_total'] + df['Port_total'] + df['Config_total']
    df = df[(df.input_ports != 1) | ((df.input_ports == 1) & (df.fw == 2))]
    df = df.drop_duplicates()

    all_categories = [
        'Port_total',
        'logic_total',
        'Config_total',
        'all_total'
    ]

    fig, ax = plt.subplots(figsize=(20, 25))

    x = np.arange(len(df['design']))
    width = 0.2
    multiplier = 0

    # Stacking the bars
    for category in all_categories:
        offset = width * multiplier
        rects = ax.bar(x + offset, df[category] / df['fw'], width, label=category)
        ax.bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax.set_xlabel('Fetch Width', fontsize=24)
    ax.set_ylabel('Power', fontsize=24)
    ax.set_title('Measurements per FW', fontsize=36)
    ax.set_xticks(x + width, df['fw'])
    ax.set_xticklabels(df['fw'], rotation=45, fontsize=20)
    plt.yticks(fontsize=20)
    # Adding legend
    plt.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_fw_sweep_grow_ports_per_port(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other'] = df['MemintfDec'] + df['MemoryPort']
    df['logic'] = df['AG'] + df['SG'] + df['ID']
    df['all'] = df['AG'] + df['SG'] + df['ID'] + df['Port'] + df['Config']
    df = df[(df.input_ports != 1) | ((df.input_ports == 1) & (df.fw == 2))]
    df = df.drop_duplicates()

    all_categories = [
        'Port',
        'logic',
        'Config',
        'all'
    ]

    fig, ax = plt.subplots(figsize=(20, 25))

    x = np.arange(len(df['design']))
    width = 0.2
    multiplier = 0

    # Stacking the bars
    for category in all_categories:
        offset = width * multiplier
        rects = ax.bar(x + offset, df[category] / df['fw'], width, label=category)
        ax.bar_label(rects, padding=2, fontsize=16)
        multiplier += 1

    # Adding labels and title
    ax.set_xlabel('Fetch Width', fontsize=24)
    ax.set_ylabel('Areas', fontsize=24)
    ax.set_title('Measurements per FW', fontsize=36)
    ax.set_xticks(x + width, df['fw'])
    ax.set_xticklabels(df['fw'], rotation=45, fontsize=20)
    plt.yticks(fontsize=20)
    # Adding legend
    plt.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_fw_sweep_grow_ports_power(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other_total'] = df['MemintfDec_total'] + df['MemoryPort_total']
    df = df[(df.input_ports != 1) | ((df.input_ports == 1) & (df.fw == 2))]
    df = df.drop_duplicates()
    # Filter to capacity 8192
    # df = df.sort_values('capacity')

    all_categories = [
        'Storage_total',
        'ID_total',
        'AG_total',
        'Config_total',
        'SG_total',
        'Port_total',
        'Other_total',
    ]

    fig, ax = plt.subplots(figsize=(20, 25))

    x = np.arange(len(df['design']))

    bottom = np.zeros(len(df['design']))
    # Stacking the bars
    for category in all_categories:
        p = ax.bar(x, df[category], width=0.5, label=category, bottom=bottom)
        bottom += df[category]

    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Power', fontsize=24)
    ax.set_title('FW/2 input FW/2 output single memory port FW sweep', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(df['fw'], rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_fw_sweep_grow_ports(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other'] = df['MemintfDec'] + df['MemoryPort']
    df = df[(df.input_ports != 1) | ((df.input_ports == 1) & (df.fw == 2))]
    df = df.drop_duplicates()
    # Filter to capacity 8192
    # df = df.sort_values('capacity')

    all_categories = [
        'Storage',
        'ID',
        'AG',
        'Config',
        'SG',
        'Port',
        'Other',
    ]

    fig, ax = plt.subplots(figsize=(20, 25))

    x = np.arange(len(df['design']))

    bottom = np.zeros(len(df['design']))
    # Stacking the bars
    for category in all_categories:
        p = ax.bar(x, df[category], width=0.5, label=category, bottom=bottom)
        bottom += df[category]

    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Areas', fontsize=24)
    ax.set_title('FW/2 input FW/2 output single memory port FW sweep', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(df['fw'], rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_fw_sweep_iso_ports_power(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other_total'] = df['MemintfDec_total'] + df['MemoryPort_total']
    df = df[(df.input_ports == 1) & (df.output_ports == 1)]
    df = df.drop_duplicates()

    all_categories = [
        'Storage_total',
        'ID_total',
        'AG_total',
        'Config_total',
        'SG_total',
        'Port_total',
        'Other_total',
    ]

    fig, ax = plt.subplots(figsize=(20, 25))

    x = np.arange(len(df['design']))

    bottom = np.zeros(len(df['design']))
    # Stacking the bars
    for category in all_categories:
        p = ax.bar(x, df[category], width=0.5, label=category, bottom=bottom)
        bottom += df[category]

    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Power', fontsize=24)
    ax.set_title('1 input 1 output single memory port FW sweep', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(df['fw'], rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_fw_sweep_iso_ports(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other'] = df['MemintfDec'] + df['MemoryPort']
    df = df[(df.input_ports == 1) & (df.output_ports == 1)]
    df = df.drop_duplicates()

    all_categories = [
        'Storage',
        'ID',
        'AG',
        'Config',
        'SG',
        'Port',
        'Other',
    ]

    fig, ax = plt.subplots(figsize=(20, 25))

    x = np.arange(len(df['design']))

    bottom = np.zeros(len(df['design']))
    # Stacking the bars
    for category in all_categories:
        p = ax.bar(x, df[category], width=0.5, label=category, bottom=bottom)
        bottom += df[category]

    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Areas', fontsize=24)
    ax.set_title('1 input 1 output single memory port FW sweep', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(df['fw'], rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_config_graph(data, filename="scatterplot"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    # design = df['design']
    Config = df['Config']
    config_size = df['config_size']

    all_datas = {
        'config_size': config_size,
        'Config': Config,

    }
    plt.figure(figsize=(10, 10))
    plt.scatter(config_size, Config, c = "blue")

    # Adding labels and title
    plt.xlabel('# Config Bits', fontsize=12)
    plt.ylabel('Config Size', fontsize=12)
    plt.title('Config size vs Config Bits', fontsize=12)
    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def convert_port_dict_list(port_dict_list, tag='static'):
    ret_list = []
    for port_name, port_bd in port_dict_list.items():
        port_bd['port_name'] = port_name
        port_bd['tag'] = tag
        ret_list.append(port_bd)
    return ret_list


def create_dual_quad_ports_two(data, all_port_lists, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other'] = df['MemintfDec'] + df['MemoryPort']
    print(df)

    # Top left dataframe is just summary of 8192 static
    static_summary = df[(df.design == 'dual_port_wide_fetch_quad') & (df.capacity == 8192)]
    static_index = df.index[(df.design == 'dual_port_wide_fetch_quad') & (df.capacity == 8192)].tolist()[0]

    # Convert the list from dict of dicts to list of dicts

    static_port_list = all_port_lists[static_index]
    static_port_df = pd.DataFrame(data=convert_port_dict_list(static_port_list, tag='static'))

    rv_summary = df[(df.design == 'dual_port_wide_fetch_quad_rv') & (df.capacity == 8192)]
    rv_index = df.index[(df.design == 'dual_port_wide_fetch_quad_rv') & (df.capacity == 8192)].tolist()[0]
    rv_port_list = all_port_lists[rv_index]
    rv_port_df = pd.DataFrame(data=convert_port_dict_list(rv_port_list, tag='rv'))

    print(static_port_df)
    print(rv_port_df)

    combo_df = pd.concat([static_port_df, rv_port_df])
    print(combo_df)

    all_categories_summary = [
        'Storage',
        'ID',
        'AG',
        'Config',
        'SG',
        'Port',
        'Other',
    ]

    all_categories_port = [
        'Storage',
        'ID',
        'AG',
        'SG',
    ]

    fig, ax = plt.subplots(1, 2, figsize=(20, 25))

    both = df[((df.design == 'dual_port_wide_fetch_quad') | (df.design == 'dual_port_wide_fetch_quad_rv')) & (df.capacity == 8192)]

    rotation = 0
    # Top left
    x = np.arange(2)
    # Handle top left (static summary)
    bottom = np.zeros(2)
    # Stacking the bars
    for category in all_categories_summary:
        p = ax[0].bar(x, both[category], width=0.5, label=category, bottom=bottom)
        bottom += both[category]

    # Adding labels and title
    ax[0].set_xlabel('Design', fontsize=12)
    ax[0].set_ylabel('Areas', fontsize=12)
    ax[0].set_title('Area Summary - 4 Port dual MemoryPort 2 input 2 output', fontsize=12)
    ax[0].set_xticks(x)
    ax[0].set_xticklabels(both['design'], rotation=rotation, fontsize=12)
    # Adding legend
    ax[0].legend(fontsize=12)

    # Top right
    x = np.arange(len(static_port_list))
    # Handle top left (static summary)
    bottom = np.zeros(len(static_port_list) * 2)
    # Stacking the bars

    width = 0.2
    multiplier = 0

    # Stacking the bars
    for category in all_categories_port:
        offset = width * multiplier
        rects = ax[1].bar(x + offset, combo_df[category], width, label=category, bottom=bottom)
        # ax.bar_label(rects, padding=2, fontsize=16)
        bottom += combo_df[category]
        multiplier += 1

    # for category in all_categories_port:
    #     p = ax[1].bar(x, combo_df[category], width=0.5, label=category, bottom=bottom)
    #     bottom += static_port_df[category]

    # Adding labels and title
    ax[1].set_xlabel('Port Instance', fontsize=12)
    ax[1].set_ylabel('Areas', fontsize=12)
    ax[1].set_title('Port Area Summary - 4 Port dual MemoryPort 2 input 2 output', fontsize=12)
    ax[1].set_xticks(x)
    ax[1].set_xticklabels(static_port_df['port_name'], rotation=rotation, fontsize=12)
    # Adding legend
    ax[1].legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_dual_quad_ports(data, all_port_lists, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df['Other'] = df['MemintfDec'] + df['MemoryPort']
    print(df)

    # Top left dataframe is just summary of 8192 static
    static_summary = df[(df.design == 'dual_port_wide_fetch_quad') & (df.capacity == 8192)]
    static_index = df.index[(df.design == 'dual_port_wide_fetch_quad') & (df.capacity == 8192)].tolist()[0]

    # Convert the list from dict of dicts to list of dicts

    static_port_list = all_port_lists[static_index]
    static_port_df = pd.DataFrame(data=convert_port_dict_list(static_port_list))

    rv_summary = df[(df.design == 'dual_port_wide_fetch_quad_rv') & (df.capacity == 8192)]
    rv_index = df.index[(df.design == 'dual_port_wide_fetch_quad_rv') & (df.capacity == 8192)].tolist()[0]
    rv_port_list = all_port_lists[rv_index]
    rv_port_df = pd.DataFrame(data=convert_port_dict_list(rv_port_list))

    all_categories_summary = [
        'Storage',
        'ID',
        'AG',
        'Config',
        'SG',
        'Port',
        'Other',
    ]

    all_categories_port = [
        'Storage',
        'ID',
        'AG',
        'SG',
    ]

    width = 1

    fig, ax = plt.subplots(2, 2, figsize=(20, 25))

    sum_ylim = 40000
    port_ylim = 1200

    rotation = 0
    # Top left
    x = np.arange(1)
    # Handle top left (static summary)
    bottom = np.zeros(1)
    # Stacking the bars
    for category in all_categories_summary:
        p = ax[0, 0].bar(x, static_summary[category], width=width, label=category, bottom=bottom)
        bottom += static_summary[category]

    # Adding labels and title
    ax[0, 0].set_xlabel('Design', fontsize=20)
    ax[0, 0].set_ylabel('Areas', fontsize=20)
    ax[0, 0].set_title('Area Summary - 4 Port\n dual MemoryPort 2 input 2 output', fontsize=20)
    ax[0, 0].set_xticks(x)
    ax[0, 0].set_xticklabels(static_summary['design'], rotation=rotation)
    # Adding legend
    ax[0, 0].legend(fontsize=20)
    ax[0, 0].set_ylim([0, sum_ylim])
    ax[0, 0].set_xlim([-1, 1])

    # plt.yticks(fontsize=20)
    # plt.legend(fontsize=20)

    # Bottom Left
    x = np.arange(len(static_port_list))
    # Handle top left (static summary)
    bottom = np.zeros(len(static_port_list))
    # Stacking the bars
    for category in all_categories_port:
        p = ax[1, 0].bar(x, static_port_df[category], width=0.5, label=category, bottom=bottom)
        bottom += static_port_df[category]

    # Adding labels and title
    ax[1, 0].set_xlabel('Port Instance', fontsize=20)
    ax[1, 0].set_ylabel('Areas', fontsize=20)
    ax[1, 0].set_title('Port Area Summary - 4 Port\n dual MemoryPort 2 input 2 output', fontsize=20)
    ax[1, 0].set_xticks(x)
    ax[1, 0].set_xticklabels(static_port_df['port_name'], rotation=rotation)
    # Adding legend
    ax[1, 0].legend(fontsize=20)
    ax[1, 0].set_ylim([0, port_ylim])

    # plt.yticks(fontsize=20)
    # plt.legend(fontsize=20)

    # Top right
    x = np.arange(1)
    # Handle top left (static summary)
    bottom = np.zeros(1)
    # Stacking the bars
    for category in all_categories_summary:
        p = ax[0, 1].bar(x, rv_summary[category], width=width, label=category, bottom=bottom)
        bottom += rv_summary[category]

    # Adding labels and title
    ax[0, 1].set_xlabel('Design', fontsize=20)
    ax[0, 1].set_ylabel('Areas', fontsize=20)
    ax[0, 1].set_title('Area Summary - 4 Port\n dual MemoryPort 2 input 2 output', fontsize=20)
    ax[0, 1].set_xticks(x)
    ax[0, 1].set_xticklabels(rv_summary['design'], rotation=rotation)
    # Adding legend
    ax[0, 1].legend(fontsize=20)
    ax[0, 1].set_ylim([0, sum_ylim])
    ax[0, 1].set_xlim([-1, 1])

    # plt.yticks(fontsize=20)
    # plt.legend(fontsize=20)

    # Bottom right
    x = np.arange(len(rv_port_list))
    # Handle top left (static summary)
    bottom = np.zeros(len(rv_port_list))
    # Stacking the bars
    for category in all_categories_port:
        p = ax[1, 1].bar(x, rv_port_df[category], width=0.5, label=category, bottom=bottom)
        bottom += rv_port_df[category]

    # Adding labels and title
    ax[1, 1].set_xlabel('Port Instance', fontsize=20)
    ax[1, 1].set_ylabel('Areas', fontsize=20)
    ax[1, 1].set_title('Port Area Summary - 4 Port\n dual MemoryPort 2 input 2 output', fontsize=20)
    ax[1, 1].set_xticks(x)
    ax[1, 1].set_xticklabels(rv_port_df['port_name'], rotation=rotation)
    # Adding legend
    ax[1, 1].legend(fontsize=20)
    ax[1, 1].set_ylim([0, port_ylim])

    # plt.yticks(fontsize=20)
    # plt.legend(fontsize=20)

    # plt.suptitle('Area breakdown ports fw', fontsize=20)
    plt.tight_layout()

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_dual_quad_summary_power(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)

    # Filter to capacity 8192
    df = df[(df.design == 'dual_port_wide_fetch_quad') | (df.design == 'dual_port_wide_fetch_quad_rv')]
    df = df.sort_values('capacity')

    design = df['design']

    all_datas = [
        'Storage_total',
        'ID_total',
        'AG_total',
        'Config_total',
        'SG_total',
        'Port_total',
        # 'Other',
    ]

    fig, ax = plt.subplots(figsize=(20, 25))
    x = np.arange(len(design))
    bottom = np.zeros(len(design))
    # Stacking the bars
    for category in all_datas:
        p = ax.bar(x, df[category], width=0.5, label=category, bottom=bottom)
        bottom += df[category]

    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Power', fontsize=24)
    ax.set_title('Power Summary - 4 Port dual MemoryPort 2 input 2 output', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(design, rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)
    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_dual_quad_summary(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)

    # quit()
    # Filter to capacity 8192
    df = df[(df.design == 'dual_port_wide_fetch_quad') | (df.design == 'dual_port_wide_fetch_quad_rv')]
    df = df.sort_values('capacity')
    print(df)
    design = df['design']
    AG = df['AG']
    SG = df['SG']
    ID = df['ID']
    Ports = df['Port']
    Storage = df['Storage']
    Config = df['Config']
    Other = df['MemintfDec'] + df['MemoryPort']

    all_datas = {
        'Storage': Storage,
        'ID': ID,
        'AG': AG,
        'Config': Config,
        'SG': SG,
        'Ports': Ports,
        'Other': Other,
    }

    fig, ax = plt.subplots(figsize=(20, 25))

    x = np.arange(len(design))

    bottom = np.zeros(len(design))
    # Stacking the bars
    for area_type, area in all_datas.items():
        p = ax.bar(x, area, width=0.5, label=area_type, bottom=bottom)
        bottom += area

    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Areas', fontsize=24)
    ax.set_title('Area Summary - 4 Port dual MemoryPort 2 input 2 output', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(design, rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)
    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_stacked_bar_power_summary(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)
    df = df.sort_values('capacity')
    # df['Other'] = df['MemintfDec'] + df['MemoryPort']
    df = df[(df.design == 'simple_dual_port')]
    # print(df)
    design = df['design']

    all_datas = [
        'Storage_total',
        'ID_total',
        'AG_total',
        'Config_total',
        'SG_total',
        'Port_total',
        # 'Other',
    ]

    fig, ax = plt.subplots(figsize=(20, 25))
    x = np.arange(len(design))
    bottom = np.zeros(len(design))
    # Stacking the bars
    for category in all_datas:
        p = ax.bar(x, df[category], width=0.5, label=category, bottom=bottom)
        # p2 = ax.bar(x + 0.5, df[category], width=0.5, label=category, bottom=bottom, color='None')
        # ax.bar_label(p, padding=5, fontsize=24, label_type='center')
        ax.bar_label(p, padding=5, fontsize=24)
        bottom += df[category]

    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Areas', fontsize=24)
    ax.set_title('Power Summary - 1 input 1 output 2 memory port (dual port)', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(design, rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)
    plt.tight_layout()
    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def create_stacked_bar_area_summary(data, filename="stacked_bar_chart"):
    # Create dataframe from dicts...
    df = pd.DataFrame(data=data)

    # Filter to capacity 8192
    df = df[(df.capacity == 8192) & (df.design != 'dual_port_wide_fetch')]
    df = df.sort_values(['memory_ports', 'input_ports', 'fw'])
    print(df)
    design = df['design']
    AG = df['AG']
    SG = df['SG']
    ID = df['ID']
    Ports = df['Port']
    Storage = df['Storage']
    Config = df['Config']
    Other = df['MemintfDec'] + df['MemoryPort']

    all_datas = {
        'Storage': Storage,
        'ID': ID,
        'AG': AG,
        'Config': Config,
        'SG': SG,
        'Ports': Ports,
        'Other': Other,
    }

    fig, ax = plt.subplots(figsize=(20, 25))
    x = np.arange(len(design))
    bottom = np.zeros(len(design))
    # Stacking the bars
    for area_type, area in all_datas.items():
        p = ax.bar(x, area, width=0.5, label=area_type, bottom=bottom)
        bottom += area
    # Adding labels and title
    ax.set_xlabel('Design', fontsize=24)
    ax.set_ylabel('Areas', fontsize=24)
    ax.set_title('Area Summary - all capacity 8192', fontsize=36)
    ax.set_xticks(x)
    ax.set_xticklabels(design, rotation=45, fontsize=20)
    # Adding legend
    ax.legend(fontsize=20)

    # Save the plot as a PNG file
    outfile = os.path.join(".", "figs", f"{filename}.png")
    plt.savefig(outfile)


def get_config_bits_verilog(all_lines):
    look_for_config_mem = False
    for i_, l_ in enumerate(all_lines):
        if look_for_config_mem:
            if " config_memory," in l_:
                # Have a hit - return the number
                cm_line_tk = l_.split()[2]
                # Have a verilog definition like [X:0] - return X + 1
                num_min1_w_brkt = cm_line_tk.split(':')[0]
                num_min1 = int(num_min1_w_brkt[1:])
                return num_min1

        if "module lakespec" in l_:
            look_for_config_mem = True


def write_area_csv(area_breakdowns, fp):
    assert len(area_breakdowns) > 0
    fp_use = "./area_breakdown.csv"
    with open(fp_use, 'w') as f_use:
        f_use.write(','.join(area_breakdowns[0].keys()))
        f_use.write('\n')
        for area_breakdown in area_breakdowns:
            f_use.write(','.join(str(x) for x in area_breakdown.values()))
            f_use.write('\n')


def write_power_csv(power_breakdowns, fp):
    assert len(power_breakdowns) > 0
    fp_use = "./area_breakdown.csv"
    with open(fp_use, 'w') as f_use:
        f_use.write(','.join(power_breakdowns[0].keys()))
        f_use.write('\n')
        for power_breakdown in power_breakdowns:
            f_use.write(','.join(str(x) for x in power_breakdown.values()))
            f_use.write('\n')


def get_manifest_info(design):
    data = None
    manifest_file_path = os.path.join(design, "params.json")
    if check_file_exists_and_has_content(manifest_file_path):
        # Load JSON from a file
        with open(manifest_file_path, 'r') as file:
            data = json.load(file)
    return data


def get_power_breakdown_dir(directory, run_power):
    # First get all designs
    all_designs = os.listdir(directory)
    all_power_breakdowns = []
    for design in all_designs:
        design_path = os.path.join(directory, design)
        design_points = os.listdir(design_path)
        for design_point in design_points:
            design_point_path = os.path.join(design_path, design_point)

            if run_power:
                subprocess.run(["make", "28"], cwd=design_point_path)

            man_info = get_manifest_info(design_point_path)
            relative_power_file_hier = os.path.join("28-synopsys-ptpx-gl",
                                                    "reports",
                                                    "lakespec.power.hier.rpt")
            relative_power_file = os.path.join("28-synopsys-ptpx-gl",
                                               "reports",
                                               "lakespec.power.rpt")

            full_power_file_hier = os.path.join(design_point_path, relative_power_file_hier)
            full_power_file = os.path.join(design_point_path, relative_power_file)
            other_info_file = os.path.join(design_point_path, "3-rtl", "outputs", "info.json")
            other_info = None
            if check_file_exists_and_has_content(other_info_file):
                with open(other_info_file, 'r') as oif:
                    other_info = json.load(oif)

            rtl_design_file = os.path.join(design_point_path, "3-rtl", "outputs", "design.v")
            rtl_lines = None
            if check_file_exists_and_has_content(rtl_design_file):
                with open(rtl_design_file, 'r') as rdf:
                    rtl_lines = rdf.readlines()
            num_cfg_bits = get_config_bits_verilog(rtl_lines)
            # print(num_cfg_bits)

            power_breakdown = get_power_breakdown_file(file_path=full_power_file_hier)
            # power_breakdown, ports_bds = get_power_breakdown_file(file_path=full_power_file_hier)
            if man_info is not None:
                # Copy over the keys for the csv
                for k, v in man_info.items():
                    power_breakdown[k] = v
            else:
                power_breakdown['design'] = f"{design}_{design_point}"

            if other_info is not None:
                # Copy over the keys for the csv
                for k, v in other_info.items():
                    power_breakdown[k] = v

            if 'config_size' not in power_breakdown and num_cfg_bits is not None:
                power_breakdown['config_size'] = num_cfg_bits

            # Now we have the parameter info and the area breakdown...add to list
            # all_power_breakdowns.append((power_breakdown, ports_bds))
            all_power_breakdowns.append(power_breakdown)
    return all_power_breakdowns


def get_power_breakdown_file(file_path):
    print(f"Getting the power at {file_path}")
    all_file_content = get_file_contents(file_path=file_path)
    # print(all_file_content)
    # quit()

    # For each category, we want internal, leakage, switching, total

    if all_file_content is None:
        print(f"No signoff area report for {file_path}")
        return {
            'total': [0.0, 0.0, 0.0, 0.0, 0.0],
            'AG': [0.0, 0.0, 0.0, 0.0, 0.0],
            'SG': [0.0, 0.0, 0.0, 0.0, 0.0],
            'ID': [0.0, 0.0, 0.0, 0.0, 0.0],
            'Port': [0.0, 0.0, 0.0, 0.0, 0.0],
            'Storage': [0.0, 0.0, 0.0, 0.0, 0.0],
            'Config': [0.0, 0.0, 0.0, 0.0, 0.0],
            'MemintfDec': [0.0, 0.0, 0.0, 0.0, 0.0],
            'MemoryPort': [0.0, 0.0, 0.0, 0.0, 0.0]
        }

    top_idx = 0
    for i_, line in enumerate(all_file_content):
        if line.startswith('lakespec'):
        # if 'lakespec' in line:
            top_idx = i_
            break

    num_lines = len(all_file_content)
    # Know where it starts now...
    top_line = all_file_content[top_idx]
    rest_of_file = all_file_content[top_idx + 1:]

    # num_data_lines = num_lines - 3
    # header = all_file_content[0]
    # dashes = all_file_content[1]
    print('\n\n\n\n\n')
    # print(top_idx)
    # print(rest_of_file)
    # Do breakdowns by AG/SG/ID/Macro/Config
    ag_match = ['port_ag_',]
    sg_match = ['port_sg_', 'rv_comp_network']
    id_match = ['port_id_',]
    port_match = ['port_inst_',]
    storage_match = ['storage',]
    memintfdec_match = ['memintfdec_inst_',]
    memoryport_match = ['memoryport_',]
    # Everything should be only 2 spaces in - so delete any line with more spaces
    all_modules = [x for x in rest_of_file if x[0] == ' ' and x[1] == ' ' and x[2] != ' ']
    top_line_breakdown = top_line.strip().split()
    # print(all_modules)
    # total_macro = float(top_line_breakdown[-2])

    ag_power = [0.0, 0.0, 0.0, 0.0, 0.0]
    sg_power = [0.0, 0.0, 0.0, 0.0, 0.0]
    id_power = [0.0, 0.0, 0.0, 0.0, 0.0]
    port_power = [0.0, 0.0, 0.0, 0.0, 0.0]
    storage_power = [0.0, 0.0, 0.0, 0.0, 0.0]
    config_power = [0.0, 0.0, 0.0, 0.0, 0.0]
    memintf_dec_power = [0.0, 0.0, 0.0, 0.0, 0.0]
    memoryport_power = [0.0, 0.0, 0.0, 0.0, 0.0]

    all_ports = {}

    # int, switch, leak, total, percent
    data_header = ['int', 'switch', 'leak', 'total', 'percent']
    data_idx = [1, 2, 3, 4, 5]
    total_power = [float(top_line_breakdown[x]) for x in data_idx]
    print(f"Design has total power {total_power}")

    data_idx = [2, 3, 4, 5, 6]
    match_idx = 0

    # Now go through and accumulate matches
    for i_, mod in enumerate(all_modules):
        mod_tokens = mod.strip().split()
        num_matches = 0

        # Happens for 0% - just set the line to all 0s
        if 'N/A' in mod:
            line_data = [0.0, 0.0, 0.0, 0.0, 0.0]
        else:
            line_data = [float(mod_tokens[x]) for x in data_idx]
        # print(line_data)

        for _ in ag_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                for z_ in range(len(data_idx)):
                    ag_power[z_] += line_data[z_]
        for _ in sg_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                for z_ in range(len(data_idx)):
                    sg_power[z_] += line_data[z_]
        for _ in id_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                # id_power += float(mod_tokens[3])
                for z_ in range(len(data_idx)):
                    id_power[z_] += line_data[z_]
        # Port is special because we want to produce a breakdown for each as well
        for _ in port_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                # port_power += float(mod_tokens[3])
                for z_ in range(len(data_idx)):
                    port_power[z_] += line_data[z_]

                # If we have a match and need a breakdown of the port,
                # we should pass the lines up to other breakdown func
                # print(mod)
                # print(all_modules[i_ + 1])
                # start_idx = get_match_index(rest_of_file, mod)
                # end_idx = get_match_index(rest_of_file, all_modules[i_ + 1])
                # print(start_idx)
                # print(end_idx)
                # port_breakdown = get_port_breakdown(rest_of_file[start_idx:end_idx])
                # print(port_breakdown)
                # all_ports[mod_tokens[0]] = port_breakdown
        memport_match = False
        for _ in memoryport_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                # memoryport_power += float(mod_tokens[3])
                for z_ in range(len(data_idx)):
                    memoryport_power[z_] += line_data[z_]

                # Both memport and storage have 'storage' in them
                memport_match = True
        for _ in storage_match:
            if _ in mod_tokens[match_idx] and memport_match is False:
                num_matches += 1
                # storage_power += float(mod_tokens[3])
                for z_ in range(len(data_idx)):
                    storage_power[z_] += line_data[z_]

        for _ in memintfdec_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                # memintf_dec_power += float(mod_tokens[3])
                for z_ in range(len(data_idx)):
                    memintf_dec_power[z_] += line_data[z_]


        assert num_matches <= 1, f"Line ({mod}) matched too many items...{num_matches}"

    config_power = [0.0, 0.0, 0.0, 0.0, 0.0]
    for z_ in range(len(data_idx)):
        config_power[z_] = total_power[z_] - (ag_power[z_] + sg_power[z_] + id_power[z_] + storage_power[z_] + port_power[z_] + memintf_dec_power[z_] + memoryport_power[z_])

    power_dict = {
        'total': total_power,
        'AG': ag_power,
        'SG': sg_power,
        'ID': id_power,
        'Port': port_power,
        'Storage': storage_power,
        'Config': config_power,
        'MemintfDec': memintf_dec_power,
        'MemoryPort': memoryport_power
    }

    # Expand all the names for the data frame usage later
    final_power_dict = {}

    for component_category, cc_pwr_brkdwn in power_dict.items():
        for zz_, brkdwn_it in enumerate(cc_pwr_brkdwn):
            final_power_dict[f"{component_category}_{data_header[zz_]}"] = brkdwn_it

    # return power_dict
    return final_power_dict


def get_area_breakdown_dir(directory):
    # First get all designs
    all_designs = os.listdir(directory)
    all_area_breakdowns = []
    for design in all_designs:
        design_path = os.path.join(directory, design)
        design_points = os.listdir(design_path)
        for design_point in design_points:
            design_point_path = os.path.join(design_path, design_point)
            man_info = get_manifest_info(design_point_path)
            relative_area_file = os.path.join("17-cadence-innovus-signoff",
                                              "reports",
                                              "signoff.area.rpt")

            full_area_file = os.path.join(design_point_path, relative_area_file)
            other_info_file = os.path.join(design_point_path, "3-rtl", "outputs", "info.json")
            other_info = None
            if check_file_exists_and_has_content(other_info_file):
                with open(other_info_file, 'r') as oif:
                    other_info = json.load(oif)

            rtl_design_file = os.path.join(design_point_path, "3-rtl", "outputs", "design.v")
            rtl_lines = None
            if check_file_exists_and_has_content(rtl_design_file):
                with open(rtl_design_file, 'r') as rdf:
                    rtl_lines = rdf.readlines()
            num_cfg_bits = get_config_bits_verilog(rtl_lines)
            # print(num_cfg_bits)

            area_breakdown, ports_bds = get_area_breakdown_file(file_path=full_area_file)
            if man_info is not None:
                # Copy over the keys for the csv
                for k, v in man_info.items():
                    area_breakdown[k] = v
            else:
                area_breakdown['design'] = f"{design}_{design_point}"

            if other_info is not None:
                # Copy over the keys for the csv
                for k, v in other_info.items():
                    area_breakdown[k] = v

            if 'config_size' not in area_breakdown and num_cfg_bits is not None:
                area_breakdown['config_size'] = num_cfg_bits

            # Now we have the parameter info and the area breakdown...add to list
            all_area_breakdowns.append((area_breakdown, ports_bds))
    return all_area_breakdowns


def get_port_breakdown(port_lines):

    # print("Port lines")
    # print(port_lines[0])
    # for l in port_lines:
    #     print(l)

    port_lines = port_lines[1:]

    port_lines = [x for x in port_lines if x[0] == ' ' and x[1] == ' ' and x[2] == ' ' and x[3] == ' ' and x[4] != ' ']

    # Match on module type
    ag_area = 0.0
    sg_area = 0.0
    id_area = 0.0
    storage_area = 0.0
    memoryport_area = 0.0

    ag_match = ['lakespec_addr_gen',]
    sg_match = ['lakespec_schedulegenerator', 'lakespec_rv_comp_nw', 'lakespec_sched_gen']
    id_match = ['lakespec_for_loop',]
    storage_match = ['Storage',]
    memoryport_match = ['lakespec_MemoryPort',]

    port_breakdown = {}

    token_idx_match = 1

    # Now go through and accumulate matches
    for i_, mod in enumerate(port_lines):
        mod_tokens = mod.strip().split()
        num_matches = 0
        for _ in ag_match:
            if _ in mod_tokens[token_idx_match]:
                num_matches += 1
                ag_area += float(mod_tokens[3])
        for _ in sg_match:
            if _ in mod_tokens[token_idx_match]:
                num_matches += 1
                sg_area += float(mod_tokens[3])
        for _ in id_match:
            if _ in mod_tokens[token_idx_match]:
                num_matches += 1
                id_area += float(mod_tokens[3])
        # Port is special because we want to produce a breakdown for each as well
        for _ in storage_match:
            if _ in mod_tokens[token_idx_match]:
                num_matches += 1
                storage_area += float(mod_tokens[3])
        for _ in memoryport_match:
            if _ in mod_tokens[token_idx_match]:
                num_matches += 1
                memoryport_area += float(mod_tokens[3])

        assert num_matches <= 1, f"Line ({mod}) matched too many items...{num_matches}"

    port_breakdown = {
        'AG': ag_area,
        'SG': sg_area,
        'ID': id_area,
        'Storage': storage_area,
        'MemoryPort': memoryport_area
    }

    return port_breakdown


def get_match_index(all_lines, line):
    for i, l in enumerate(all_lines):
        if line == l:
            return i
    return None


def get_area_breakdown_file(file_path):
    print(f"Getting the area at {file_path}")
    all_file_content = get_file_contents(file_path=file_path)
    if all_file_content is None:
        print(f"No signoff area report for {file_path}")
        return {
        'total': 0.0,
        'AG': 0.0,
        'SG': 0.0,
        'ID': 0.0,
        'Port': 0.0,
        'Storage': 0.0,
        'Config': 0.0,
        'MemintfDec': 0.0,
        'MemoryPort': 0.0
    }
    num_lines = len(all_file_content)
    num_data_lines = num_lines - 3
    header = all_file_content[0]
    dashes = all_file_content[1]
    top_line = all_file_content[2]
    rest_of_file = all_file_content[3:]
    # Do breakdowns by AG/SG/ID/Macro/Config
    ag_match = ['port_ag_',]
    sg_match = ['port_sg_', 'rv_comp_network']
    id_match = ['port_id_',]
    port_match = ['port_inst_',]
    storage_match = ['storage',]
    memintfdec_match = ['memintfdec_inst_',]
    memoryport_match = ['memoryport_',]
    # Everything should be only 2 spaces in - so delete any line with more spaces
    all_modules = [x for x in rest_of_file if x[0] == ' ' and x[1] == ' ' and x[2] != ' ']
    top_line_breakdown = top_line.strip().split()
    total_area = float(top_line_breakdown[2])
    total_macro = float(top_line_breakdown[-2])
    print(f"Design has total area with {total_area} and macro area of {total_macro} meaning a {total_macro * 100.0 / total_area}% storage")

    ag_area = 0.0
    sg_area = 0.0
    id_area = 0.0
    port_area = 0.0
    storage_area = 0.0
    config_area = 0.0
    memintf_dec_area = 0.0
    memoryport_area = 0.0

    all_ports = {}

    match_idx = 0

    # Now go through and accumulate matches
    for i_, mod in enumerate(all_modules):
        mod_tokens = mod.strip().split()
        num_matches = 0
        for _ in ag_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                ag_area += float(mod_tokens[3])
        for _ in sg_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                sg_area += float(mod_tokens[3])
        for _ in id_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                id_area += float(mod_tokens[3])
        # Port is special because we want to produce a breakdown for each as well
        for _ in port_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                port_area += float(mod_tokens[3])
                # If we have a match and need a breakdown of the port,
                # we should pass the lines up to other breakdown func
                # print(mod)
                # print(all_modules[i_ + 1])
                start_idx = get_match_index(rest_of_file, mod)
                end_idx = get_match_index(rest_of_file, all_modules[i_ + 1])
                # print(start_idx)
                # print(end_idx)
                port_breakdown = get_port_breakdown(rest_of_file[start_idx:end_idx])
                # print(port_breakdown)
                all_ports[mod_tokens[0]] = port_breakdown
        memport_match = False
        for _ in memoryport_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                memoryport_area += float(mod_tokens[3])
                # Both memport and storage have 'storage' in them
                memport_match = True
        for _ in storage_match:
            if _ in mod_tokens[match_idx] and memport_match is False:
                num_matches += 1
                storage_area += float(mod_tokens[3])
        for _ in memintfdec_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                memintf_dec_area += float(mod_tokens[3])

        assert num_matches <= 1, f"Line ({mod}) matched too many items...{num_matches}"

    config_area = total_area - (ag_area + sg_area + id_area + storage_area + port_area + memintf_dec_area + memoryport_area)

    area_dict = {
        'total': total_area,
        'AG': ag_area,
        'SG': sg_area,
        'ID': id_area,
        'Port': port_area,
        'Storage': storage_area,
        'Config': config_area,
        'MemintfDec': memintf_dec_area,
        'MemoryPort': memoryport_area
    }

    return area_dict, all_ports


def get_num_live_procs(proc_list):
    # Wait for all to be done.
    num_procs_alive = 0
    done = False
    while not done:
        done = True
        num_procs_alive = 0
        for proc_ in proc_list:
            # Still an alive process...
            if proc_.poll() is None:
                num_procs_alive += 1
    return num_procs_alive


if __name__ == "__main__":

    area_report_labels = ["Hinst Name", "Module Name", "Inst Count", "Total Area", "Buffer",
                          "Inverter", "Combinational", "Flop", "Latch", "Clock Gate", "Macro", "Physical"]

    parser = argparse.ArgumentParser(description='Generating experiments')
    parser.add_argument("--collect_data", action="store_true")
    parser.add_argument("--collect_override", action="store_true")
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--run_builds", action="store_true")
    # parser.add_argument("--collect_power", action="store_true")
    parser.add_argument("--run_power", action="store_true")
    parser.add_argument("--build_dir", type=str, default=None, required=True)
    parser.add_argument("--csv_out", type=str, default=None, required=False)
    parser.add_argument("--design_filter", type=str, default=None, required=False)
    parser.add_argument("--experiment", type=str, default="summary", required=False)
    parser.add_argument("--report_path", type=str, default=None, required=False)
    parser.add_argument("--figure_name", type=str, default=None, required=False)
    parser.add_argument('--storage_capacity', nargs='*', type=int)
    parser.add_argument('--dimensionality', nargs='*', type=int)
    parser.add_argument('--data_width', nargs='*', type=int)
    parser.add_argument('--in_ports', type=int, default=1)
    parser.add_argument('--out_ports', type=int, default=1)
    parser.add_argument("--use_ports", action="store_true")
    parser.add_argument('--fetch_width', nargs='*', type=int)
    parser.add_argument('--clock_count_width', nargs='*', type=int)
    # Single port sram type (where relevant)
    parser.add_argument("--spst", type=str, default=None, required=False)
    args = parser.parse_args()
    physical_arg = args.physical
    run_builds = args.run_builds
    pd_build_dir = args.build_dir
    collect_data = args.collect_data
    collect_override = args.collect_override
    collect_override_path = pd_build_dir
    collect_data_csv_path = args.csv_out
    design_filter = args.design_filter
    inp = args.in_ports
    outp = args.out_ports
    use_ports = args.use_ports
    report_path = args.report_path
    figure_name = args.figure_name
    experiment = args.experiment
    run_power = args.run_power
    # collect_power = args.collect_power

    spst = args.spst

    # Matches everything
    if design_filter is None:
        design_filter = ""

    if collect_data is False:
        assert pd_build_dir is not None, f"If not collecting data, must provide a build dir!!!"

    x_or_more = 4

    # all test files...
    base_dir = os.path.dirname(os.path.abspath(__file__))
    lake_base_dir = os.path.join(base_dir, "../")
    test_files_dir = os.path.join(lake_base_dir, "tests/test_spec/")
    pd_files_dir = os.path.join(lake_base_dir, "gf_physical_design/NEW/")
    make_script = os.path.join(lake_base_dir, "gf_physical_design/", "make_all.sh")

    # If the collect data flag is on, all else is ignored, and we will go collect all the
    # data from the build dir
    if collect_data:
        if collect_override:
            params_dict = {
                "design": "mek",
                "frequency": 1000,
                "capacity": 1024,
                "data_width": 16,
                "clock_count_width": 64,
                "dimensionality": 6
                }

            # Dump a parameter file so that we can understand what the design has in it
            params_file = os.path.join(collect_override_path, "params.json")
            with open(params_file, 'w') as json_file:
                json.dump(params_dict, json_file, indent=4)

            with open(params_file, 'r') as file:
                data = json.load(file)
            # area_report = os.path.join(collect_override_path, "signoff.area.rpt")
            area_report = report_path
            area_dict, all_ports_bd = get_area_breakdown_file(file_path=area_report)
            # print(area_dict)
            # print(all_ports_bd)
            create_stacked_bar_area_summary(area_dict)

        else:
            print(f"Data collection enabled at build dir {pd_build_dir}...")
            # Consists of (summary, [ports])
            all_breakdowns = get_area_breakdown_dir(pd_build_dir)
            all_breakdowns_power = get_power_breakdown_dir(pd_build_dir, run_power)
            # Now emit this information to excel
            assert collect_data_csv_path is not None
            all_summaries = []
            all_port_lists = []
            for summary_bd, port_db_list in all_breakdowns:
                all_summaries.append(summary_bd)
                all_port_lists.append(port_db_list)
            write_area_csv(all_summaries, collect_data_csv_path)
            # Do different experiments here...
            if experiment == "summary":
                print("Creating area summary at 8192 size...")
                create_stacked_bar_area_summary(all_summaries, filename=figure_name)
            elif experiment == "dual_quad_summary":
                create_dual_quad_summary(data=all_summaries, filename=figure_name)
            elif experiment == "dual_quad_ports":
                create_dual_quad_ports(data=all_summaries, all_port_lists=all_port_lists, filename=figure_name)
            elif experiment == "config":
                create_config_graph(data=all_summaries, filename=figure_name)
            elif experiment == "fw_sweep_iso_ports":
                create_fw_sweep_iso_ports(data=all_summaries, filename=figure_name)
            elif experiment == "fw_sweep_grow_ports":
                create_fw_sweep_grow_ports(data=all_summaries, filename=figure_name)
            elif experiment == "fw_sweep_grow_ports_per_port":
                create_fw_sweep_grow_ports_per_port(data=all_summaries, filename=figure_name)
            elif experiment == "spst":
                create_spst(data=all_summaries, filename=figure_name)
            elif experiment == "control_v_capacity":
                create_control_v_capacity(data=all_summaries, filename=figure_name)
            elif experiment == "control_v_runtime":
                create_control_v_runtime(data=all_summaries, filename=figure_name)
            elif experiment == "summary_power":
                print("Creating power summary size...")
                create_stacked_bar_power_summary(data=all_breakdowns_power, filename=figure_name)
            elif experiment == "dual_quad_summary_power":
                create_dual_quad_summary_power(data=all_breakdowns_power, filename=figure_name)
            # elif experiment == "dual_quad_ports_power":
                # create_dual_quad_ports_power(data=all_breakdowns, all_port_lists=all_port_lists, filename=figure_name)
            elif experiment == "fw_sweep_iso_ports_power":
                create_fw_sweep_iso_ports_power(data=all_breakdowns_power, filename=figure_name)
            elif experiment == "fw_sweep_grow_ports_power":
                create_fw_sweep_grow_ports_power(data=all_breakdowns_power, filename=figure_name)
            elif experiment == "fw_sweep_grow_ports_per_port_power":
                create_fw_sweep_grow_ports_per_port_power(data=all_breakdowns_power, filename=figure_name)
            elif experiment == "spst_power":
                create_spst_power(data=all_breakdowns_power, filename=figure_name)
            elif experiment == "control_v_capacity_power":
                create_control_v_capacity_power(data=all_breakdowns_power, filename=figure_name)
            elif experiment == "control_v_runtime_power":
                create_control_v_runtime_power(data=all_breakdowns_power, filename=figure_name)
            else:
                raise ValueError(f"Doesn't support experiment: {experiment}")

        exit()

    dimensionalities_use = [6]
    scale_value = 8
    storage_capacity_use = [512 * scale_value, 1024 * scale_value, 2048 * scale_value]
    # storage_capacity_use = [16384]
    data_width_use = [16]
    ccw_use = [64]
    fetch_width_use = [4]

    storage_capacity_arg = args.storage_capacity
    if (storage_capacity_arg is not None) and len(storage_capacity_arg) > 0:
        print(f"Overriding used storage_cap of {storage_capacity_use} with {storage_capacity_arg}")
        storage_capacity_use = storage_capacity_arg
    data_width_arg = args.data_width
    if (data_width_arg is not None) and len(data_width_arg) > 0:
        print(f"Overriding used data_width of {data_width_use} with {data_width_arg}")
        data_width_use = data_width_arg
    ccw_arg = args.clock_count_width
    if (ccw_arg is not None) and len(ccw_arg) > 0:
        print(f"Overriding used ccw of {ccw_use} with {ccw_arg}")
        ccw_use = ccw_arg
    dim_arg = args.dimensionality
    if (dim_arg is not None) and len(dim_arg) > 0:
        print(f"Overriding used dimensionality of {dimensionalities_use} with {dim_arg}")
        dimensionalities_use = dim_arg
    fetch_width_arg = args.fetch_width
    if (fetch_width_arg is not None) and len(fetch_width_arg) > 0:
        print(f"Overriding used storage_cap of {fetch_width_use} with {fetch_width_arg}")
        fetch_width_use = fetch_width_arg

    add_fw_arg = fetch_width_arg is not None
    add_port_arg = use_ports

    create_curr_dir = os.path.dirname(os.path.abspath(__file__))

    all_procs = []

    for freq in [1000]:

        all_test_files = os.listdir(test_files_dir)

        filtered_files = [f for f in all_test_files if design_filter in f]

        for filename in filtered_files:
            filename_no_ext = os.path.splitext(filename)[0]
            # if filename_no_ext not in ["dual_port_rv", "simple_dual_port"]:
                # continue
            filename_no_ext_f = f"{filename_no_ext}_{freq}"

            head_folder = os.path.join(pd_files_dir, filename_no_ext_f)
            # subprocess.run(["rm", "-rf", head_folder])
            subprocess.run(["mkdir", "-p", head_folder])

            other_folder = os.path.join(pd_build_dir, filename_no_ext_f)
            subprocess.run(["mkdir", "-p", other_folder])

            all_test_pts = ((sc, dataw, ccw, dimw, fw) for sc in storage_capacity_use for dataw in data_width_use for ccw in ccw_use for dimw in dimensionalities_use for fw in fetch_width_use)

            for (storage_capacity, data_width, clock_count_width, dimensionality, fw) in all_test_pts:

            # Now go through the different data points
            # for storage_capacity in [512 * cap_scale, 1024 * cap_scale, 2048 * cap_scale]:
            #     for data_width in [16]:
            #         for clock_count_width in [64]:

                params_dict = {
                    "design": filename_no_ext,
                    "frequency": freq,
                    "capacity": storage_capacity,
                    "data_width": data_width,
                    "clock_count_width": clock_count_width,
                    "dimensionality": dimensionality,
                    "spst": spst
                }

                design_folder = f"storage_cap_{storage_capacity}_data_width_{data_width}_ccw_{clock_count_width}_dim_{dimensionality}"

                if add_fw_arg:
                    design_folder += f"_fw_{fw}"
                if add_port_arg:
                    design_folder += f"_inp_{inp}_outp_{outp}"
                if spst is not None:
                    design_folder += f"_spst_{spst}"
                full_design_path = os.path.join(head_folder, f"{design_folder}_{freq}")

                subprocess.run(["rm", "-rf", full_design_path])
                sample_folder = os.path.join(pd_files_dir, "sample")
                subprocess.run(["cp", "-r", sample_folder, full_design_path])
                print(f"Made design folder at {full_design_path}")

                pd_build_path = os.path.join(pd_build_dir, filename_no_ext_f, design_folder)
                subprocess.run(["mkdir", "-p", pd_build_path])
                subprocess.run(["cd", pd_build_path])

                # Dump a parameter file so that we can understand what
                # the design looks like
                params_file = os.path.join(pd_build_path, "params.json")
                with open(params_file, 'w') as json_file:
                    json.dump(params_dict, json_file, indent=4)

                with open(f"{full_design_path}/rtl/configure.yml", 'w+') as rtl_configure:
                    rtl_configure.write("name: rtl\n")
                    rtl_configure.write("\n")
                    rtl_configure.write("outputs:\n")
                    rtl_configure.write("  - design.v\n")
                    rtl_configure.write("  - testbench.sv\n")
                    rtl_configure.write("  - design.args\n")
                    rtl_configure.write("  - comp_args.txt\n")
                    rtl_configure.write("  - PARGS.txt\n")
                    rtl_configure.write("  - bitstream.bs\n")
                    rtl_configure.write("  - gold\n")
                    rtl_configure.write("\n")
                    rtl_configure.write("commands:\n")
                    rtl_configure.write("\n")
                    rtl_configure.write("  - export CURR=$PWD\n")
                    rtl_configure.write("  - echo $CURR\n")
                    rtl_configure.write("\n")
                    rtl_configure.write("  - export TOP=$PWD\n")
                    rtl_configure.write("\n")

                    python_command = f"  - python {os.path.join(create_curr_dir, 'create_all_experiments.py')} --physical --storage_capacity {storage_capacity} --clock_count_width {clock_count_width} --data_width {data_width} --outdir $TOP/TEST/ --design_filter {filename_no_ext}"

                    if add_fw_arg:
                        python_command = " ".join([python_command, "--fetch_width", f"{fw}"])

                    if add_port_arg:
                        python_command = " ".join([python_command, "--in_ports", f"{inp}"])
                        python_command = " ".join([python_command, "--out_ports", f"{outp}"])
                        python_command = " ".join([python_command, "--use_ports"])

                    if spst is not None:
                        python_command = " ".join([python_command, "--spst", f"{spst}"])

                    rtl_configure.write(f"{python_command}\n")
                    rtl_configure.write("\n")
                    rtl_configure.write("  - cd $CURR\n")
                    rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/lakespec.sv outputs/design.v\n")
                    rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/info.json outputs/info.json\n")
                    rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/tb.sv outputs/testbench.sv\n")
                    rtl_configure.write(f"  - cat $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/comp_args.txt $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/PARGS.txt > outputs/design.args\n")
                    rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/comp_args.txt outputs/comp_args.txt\n")
                    rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/PARGS.txt outputs/PARGS.txt\n")
                    rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/bitstream.bs outputs/bitstream.bs\n")
                    rtl_configure.write(f"  - cp -r $TOP/TEST/{filename_no_ext}/{design_folder}/gold outputs/gold\n")
                    rtl_configure.write("\n")
                    # Make sure the test dir is set to the proper directory!!!!
                    test_dir_arg = os.path.join("TEST", filename_no_ext, design_folder)
                    rtl_configure.write(f"  - python set_test_dir.py --test_dir {test_dir_arg}\n")
                    rtl_configure.write("  - echo $PWD\n")

                print(f"cd {pd_build_path}; mflowgen run --design {full_design_path}")
                subprocess.run(["mflowgen", "run", "--design", full_design_path], cwd=pd_build_path)

                # If the builds should go, start it here...
                if run_builds is True:
                    print(f"Starting build at {pd_build_path}")
                    # execute_str = ["source", make_script]
                    # execute_str = ["make", "6", "&&", "make", "-t", "6", "&&", "make", "17"]
                    execute_str = "make 6; make -t 6; make 17; make 28;"
                    newp = subprocess.Popen(execute_str, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, cwd=pd_build_path, shell=True)
                    all_procs.append(newp)

                # print(f"Made PD build folder at {pd_build_path}")

                # Check if there are 4 or more current builds...
                if run_builds is True:
                    num_alive = get_num_live_procs(all_procs)
                    print(f"This many running...{num_alive}...")
                    while num_alive >= x_or_more:
                        print(f"At limit of {x_or_more} procs...sleeping")
                        time.sleep(15)
                        num_alive = get_num_live_procs(all_procs)

    # Wait for all to be done.
    done = False
    while not done:
        done = True
        num_procs_alive = 0
        for proc_ in all_procs:
            # Still an alive process...
            if proc_.poll() is None:
                num_procs_alive += 1
        if num_procs_alive > 0:
            print(f"{num_procs_alive} processes still running...")
            time.sleep(10)
            done = False
