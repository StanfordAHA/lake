import csv
import matplotlib
matplotlib.use('Agg')  # This selects a non-interactive backend that does not display anything
import matplotlib.pyplot as plt
import numpy as np
import os

def process_heir_power_report(input_file_path, output_csv_path):
    # Read the input file
    with open(input_file_path, 'r') as file:
        power_report_lines = file.readlines()

    # Initialize a variable to hold the accumulated sum of the fourth column values
    total_power_ag = 0.0
    total_power_sg = 0.0
    total_power_id = 0.0
    clock_power = 0.0
    storage_power = 0.0

    # Process the lines, skipping the first two rows and write to a CSV
    with open(output_csv_path, mode='w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        
        # Write header (first line) after stripping and splitting on multiple spaces
        csv_writer.writerow([x.strip() for x in power_report_lines[0].split() if x.strip()])
        
        # Process each line starting from the third line
        for line in power_report_lines[15:-1]:
            if line.strip():  # Skip any potentially empty lines
                # Split the line on one or more spaces, and strip each element to clean up
                row_data = [x.strip() for x in line.split() if x.strip()]
                # Check if the first column contains "ag" and second contains "AddressGenerator"
                if "port_ag" in row_data[0]:
                    try:
                        total_power_ag += float(row_data[5])
                    except ValueError:
                        pass  # Ignore rows where conversion to float fails
                if "port_sg" in row_data[0]:
                    try:
                        total_power_sg += float(row_data[5])
                    except ValueError:
                        pass
                if "port_id" in row_data[0]:
                    try:
                        total_power_id += float(row_data[5])
                    except ValueError:
                        pass
                ## subtract clock related power from breakdowns
                # print(row_data)
                if "CLOCK_GATE" in row_data[1] and "IterationDomain" in row_data[1]:
                    try:
                        total_power_id -= float(row_data[5])
                    except ValueError:
                        pass
                if "CLOCK_GATE" in row_data[1] and "ScheduleGenerator" in row_data[1]:
                    try:
                        total_power_sg -= float(row_data[5])
                    except ValueError:
                        pass
                if "CLOCK_GATE" in row_data[1] and "AddressGenerator" in row_data[1]:
                    try:
                        total_power_ag -= float(row_data[5])
                    except ValueError:
                        pass

                if "CLOCK_GATE" in row_data[1]:
                    try:
                        clock_power += float(row_data[5])
                    except ValueError:
                        pass
                if "storage" == row_data[0]:
                    try:
                        storage_power += float(row_data[5])
                    except ValueError:
                        pass
                csv_writer.writerow(row_data)

    # Data for plotting
    labels = ['Address\nGenerator', 'Schedule\nGenerator', 'Iteration\nDomain']
    sizes = [total_power_ag, total_power_sg/10, total_power_id]  # example data values
    colors = ['#ADD8E6','#0073E6','#003366']

    # Check if the 'images/' directory exists, and create it if it does not
    if not os.path.exists('images'):
        os.makedirs('images')

    # Bar graph using Matplotlib for control breakdown
    plt.figure(figsize=(8, 8))
    plt.bar(labels, sizes, color=colors)
    plt.ylabel('Power (mW)')
    plt.title('Power Distribution')
    bar_chart_path = './images/power_bar_chart_control_breakdown.png'
    plt.savefig(bar_chart_path)

    # Pie chart using Matplotlib for control breakdown
    plt.figure(figsize=(12, 8))  # Adjusting figure size to prevent cropping
    plt.pie(sizes, labels=labels, colors=colors, autopct='%1.1f%%', startangle=90,
            textprops={'fontsize': 34})  # Increase fontsize for labels
    plt.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.
    # plt.title("power Distribution", fontsize=34)  # Also increasing the title fontsize
    plt.tight_layout()  # Automatically adjust subplot parameters to give specified padding
    pie_chart_path = './images/power_pie_chart_control_breakdown.png'
    plt.savefig(pie_chart_path)


    # Data for plotting
    labels_totals = ['Storage', 'Control', 'Clock']
    sizes_totals = [storage_power, (total_power_ag+total_power_sg+total_power_id), clock_power]  # example data values
    colors_totals = ['#ff9999','#66b3ff','#99ff99']

    # Bar graph using Matplotlib for total breakdown
    plt.figure(figsize=(8, 8))
    plt.bar(labels_totals, sizes_totals, color=colors_totals)
    plt.ylabel('Power (mW)')
    plt.title('Power Distribution')
    bar_chart_path = './images/power_bar_chart_total_breakdown.png'
    plt.savefig(bar_chart_path)

    # Pie chart using Matplotlib for total breakdown
    plt.figure(figsize=(12, 8))  # Adjusting figure size to prevent cropping
    plt.pie(sizes_totals, labels=labels_totals, colors=colors_totals, autopct='%1.1f%%', startangle=90,
            textprops={'fontsize': 34})  # Increase fontsize for labels
    plt.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.
    # plt.title("power Distribution", fontsize=34)  # Also increasing the title fontsize
    plt.tight_layout()  # Automatically adjust subplot parameters to give specified padding
    pie_chart_path = './images/power_pie_chart_total_breakdown.png'
    plt.savefig(pie_chart_path)




    # Plotting data for the total breakdown with stacked bar
    labels_totals = ['Storage', 'Control', 'Clock']
    control_sizes = [total_power_ag, total_power_sg, total_power_id]
    sizes_totals = [clock_power, sum(control_sizes), storage_power]
    colors_control = ['#ADD8E6','#0073E6','#003366']
    colors_totals = ['#ff9999','#66b3ff','#99ff99']

    # Create a bar graph for the total breakdown
    fig, ax = plt.subplots(figsize=(8, 8))
    bars = np.add(total_power_ag, total_power_sg).tolist()
    
    ax.bar(labels_totals[0], storage_power, color=colors_totals[0], label='Storage')
    ax.bar(labels_totals[1], total_power_ag, color=colors_control[2], label='AddressGenerator')
    ax.bar(labels_totals[1], total_power_sg, bottom=total_power_ag, color=colors_control[1], label='ScheduleGenerator')
    ax.bar(labels_totals[1], total_power_id, bottom=bars, color=colors_control[0], label='IterationDomain')
    ax.bar(labels_totals[2], clock_power, color=colors_totals[2], label='Clock')

    ax.set_ylabel('Power (mW)')
    ax.set_title('Power Distribution')
    ax.legend()

    plt.savefig('./images/power_bar_chart_total_breakdown_stacked.png')
    plt.close()

    # Print the total power accumulated
    print(f"Total accumulated power ag: {total_power_ag}\n")
    print(f"Total accumulated power sg: {total_power_sg}\n")
    print(f"Total accumulated power id: {total_power_id}\n")
    print(f"Total accumulated power clock: {clock_power}\n")
    print(f"Total accumulated power storage: {storage_power}\n")




def process_power_report(input_file_path, output_csv_path):
    # Read the input file
    with open(input_file_path, 'r') as file:
        power_report_lines = file.readlines()

    # Initialize a variable to hold the accumulated sum of the fourth column values
    total_power_clock = 0.0
    total_power_reg = 0.0
    total_power_comb = 0.0

    # Process the lines, skipping the first two rows and write to a CSV
    with open(output_csv_path, mode='w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        
        # Write header (first line) after stripping and splitting on multiple spaces
        csv_writer.writerow([x.strip() for x in power_report_lines[0].split() if x.strip()])
        
        # Process each line starting from the third line
        for line in power_report_lines[15:-1]:
            if line.strip():  # Skip any potentially empty lines
                # Split the line on one or more spaces, and strip each element to clean up
                row_data = [x.strip() for x in line.split() if x.strip()]

                if "clock_network" in row_data[0]:
                    try:
                        total_power_clock += float(row_data[4])
                    except ValueError:
                        pass
                if "register" in row_data[0]:
                    try:
                        total_power_reg += float(row_data[4])
                    except ValueError:
                        pass
                if "combinational" in row_data[0]:
                    try:
                        total_power_comb += float(row_data[4])
                    except ValueError:
                        pass
                csv_writer.writerow(row_data)

    # Data for plotting
    labels = ['Clock\nNetwork', 'Register', 'Combinational']
    sizes = [total_power_clock, total_power_reg, total_power_comb]  # example data values
    colors = ['#ADD8E6','#0073E6','#003366']

    # Check if the 'images/' directory exists, and create it if it does not
    if not os.path.exists('images'):
        os.makedirs('images')

    colors_totals = ['#ff9999','#66b3ff','#99ff99']

    # Bar graph using Matplotlib for total breakdown
    plt.figure(figsize=(8, 8))
    plt.bar(labels, sizes, color=colors_totals)
    plt.ylabel('Power (mW)')
    plt.title('Power Distribution')
    bar_chart_path = './images/power_bar_chart_total_breakdown.png'
    plt.savefig(bar_chart_path)

    # Pie chart using Matplotlib for total breakdown
    plt.figure(figsize=(12, 8))  # Adjusting figure size to prevent cropping
    plt.pie(sizes, labels=labels, colors=colors_totals, autopct='%1.1f%%', startangle=90,
            textprops={'fontsize': 34})  # Increase fontsize for labels
    plt.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.
    # plt.title("power Distribution", fontsize=34)  # Also increasing the title fontsize
    plt.tight_layout()  # Automatically adjust subplot parameters to give specified padding
    pie_chart_path = './images/power_pie_chart_total_breakdown.png'
    plt.savefig(pie_chart_path)

    # Print the total power accumulated
    print(f"Total accumulated power register: {total_power_id}\n")
    print(f"Total accumulated power clock: {clock_power}\n")
    print(f"Total accumulated power combinational: {storage_power}\n")


input_file_path = './inputs/power.heir.rpt'  # specify your input file path here
output_csv_path = './filtered_heir_power_report.csv'  # specify your output file path here
process_heir_power_report(input_file_path, output_csv_path)

input_file_path = './inputs/power.rpt'  # specify your input file path here
output_csv_path = './filtered_power_report.csv'  # specify your output file path here
process_power_report(input_file_path, output_csv_path)