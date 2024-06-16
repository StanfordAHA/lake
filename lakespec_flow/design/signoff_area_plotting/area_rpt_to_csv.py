import csv
import matplotlib
matplotlib.use('Agg')  # This selects a non-interactive backend that does not display anything
import matplotlib.pyplot as plt
import numpy as np
import os

def process_area_report(input_file_path, output_csv_path):
    # Read the input file
    with open(input_file_path, 'r') as file:
        area_report_lines = file.readlines()

    # Initialize a variable to hold the accumulated sum of the fourth column values
    total_area_ag = 0.0
    total_area_sg = 0.0
    total_area_id = 0.0

    # Process the lines, skipping the first two rows and write to a CSV
    with open(output_csv_path, mode='w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        
        # Write header (first line) after stripping and splitting on multiple spaces
        csv_writer.writerow([x.strip() for x in area_report_lines[0].split() if x.strip()])
        
        # Process each line starting from the third line
        for line in area_report_lines[2:]:
            if line.strip():  # Skip any potentially empty lines
                # Split the line on one or more spaces, and strip each element to clean up
                row_data = [x.strip() for x in line.split() if x.strip()]
                # Check if the first column contains "ag" and second contains "AddressGenerator"
                if "ag" in row_data[0] and "AddressGenerator" in row_data[1]:
                    try:
                        total_area_ag += float(row_data[3])
                    except ValueError:
                        pass  # Ignore rows where conversion to float fails
                if "sg" in row_data[0] and "ScheduleGenerator" in row_data[1]:
                    try:
                        total_area_sg += float(row_data[3])
                    except ValueError:
                        pass
                if "id" in row_data[0] and "IterationDomain" in row_data[1]:
                    try:
                        total_area_id += float(row_data[3])
                    except ValueError:
                        pass
                csv_writer.writerow(row_data)

    # Data for plotting
    labels = ['Address\nGenerator', 'Schedule\nGenerator', 'Iteration\nDomain']
    sizes = [total_area_ag, total_area_sg/10, total_area_id]  # example data values
    colors = ['#ff9999','#66b3ff','#99ff99']

    # Check if the 'images/' directory exists, and create it if it does not
    if not os.path.exists('images'):
        os.makedirs('images')

    # Bar graph using Matplotlib
    plt.figure(figsize=(8, 4))
    plt.bar(labels, sizes, color=colors)
    plt.ylabel('Area (mm²)')
    plt.title('Area Distribution Bar Chart')
    bar_chart_path = './images/area_bar_chart.png'
    plt.savefig(bar_chart_path)

    # Pie chart using Matplotlib
    plt.figure(figsize=(12, 8))  # Adjusting figure size to prevent cropping
    plt.pie(sizes, labels=labels, colors=colors, autopct='%1.1f%%', startangle=90,
            textprops={'fontsize': 34})  # Increase fontsize for labels
    plt.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.
    plt.title("Area Distribution", fontsize=34)  # Also increasing the title fontsize
    plt.tight_layout()  # Automatically adjust subplot parameters to give specified padding
    pie_chart_path = './images/area_pie_chart.png'
    plt.savefig(pie_chart_path)

    # Print the total area accumulated
    print(f"Total accumulated area ag: {total_area_ag}\n")
    print(f"Total accumulated area sg: {total_area_sg}\n")
    print(f"Total accumulated area id: {total_area_id}\n")

# Example usage
input_file_path = './inputs/signoff.area.rpt'  # specify your input file path here
output_csv_path = './filtered_area_report.csv'  # specify your output file path here
process_area_report(input_file_path, output_csv_path)
