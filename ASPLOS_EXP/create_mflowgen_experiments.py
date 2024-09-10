import os
import subprocess
import argparse
import time
import json
from lake.utils.util import get_file_contents, check_file_exists_and_has_content


def write_area_csv(area_breakdowns, fp):
    assert len(area_breakdowns) > 0
    fp_use = "./area_breakdown.csv"
    with open(fp_use, 'w') as f_use:
        f_use.write(','.join(area_breakdowns[0].keys()))
        f_use.write('\n')
        for area_breakdown in area_breakdowns:
            f_use.write(','.join(str(x) for x in area_breakdown.values()))
            f_use.write('\n')


def get_manifest_info(design):
    data = None
    manifest_file_path = os.path.join(design, "params.json")
    if check_file_exists_and_has_content(manifest_file_path):
        # Load JSON from a file
        with open(manifest_file_path, 'r') as file:
            data = json.load(file)
    return data


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
            area_breakdown = get_area_breakdown_file(file_path=full_area_file)
            if man_info is not None:
                area_breakdown['design'] = man_info['design']
            else:
                area_breakdown['design'] = f"{design}_{design_point}"
            # Now we have the parameter info and the area breakdown...add to list
            all_area_breakdowns.append(area_breakdown)
    return all_area_breakdowns


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
        'Config': 0.0
    }
    num_lines = len(all_file_content)
    num_data_lines = num_lines - 3
    header = all_file_content[0]
    dashes = all_file_content[1]
    top_line = all_file_content[2]
    rest_of_file = all_file_content[3:]
    # Do breakdowns by AG/SG/ID/Macro/Config
    ag_match = ['port_ag_',]
    sg_match = ['port_sg_',]
    id_match = ['port_id_',]
    port_match = ['port_inst_',]
    storage_match = ['storage',]
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

    # Now go through and accumulate matches
    for mod in all_modules:
        mod_tokens = mod.strip().split()
        num_matches = 0
        for _ in ag_match:
            if _ in mod_tokens[0]:
                num_matches += 1
                ag_area += float(mod_tokens[3])
        for _ in sg_match:
            if _ in mod_tokens[0]:
                num_matches += 1
                sg_area += float(mod_tokens[3])
        for _ in id_match:
            if _ in mod_tokens[0]:
                num_matches += 1
                id_area += float(mod_tokens[3])
        for _ in port_match:
            if _ in mod_tokens[0]:
                num_matches += 1
                port_area += float(mod_tokens[3])
        for _ in storage_match:
            if _ in mod_tokens[0]:
                num_matches += 1
                storage_area += float(mod_tokens[3])

        assert num_matches <= 1, f"Line ({mod}) matched too many items...{num_matches}"

    config_area = total_area - (ag_area + sg_area + id_area + storage_area)

    area_dict = {
        'total': total_area,
        'AG': ag_area,
        'SG': sg_area,
        'ID': id_area,
        'Port': port_area,
        'Storage': storage_area,
        'Config': config_area
    }

    return area_dict


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
    parser.add_argument("--build_dir", type=str, default=None, required=True)
    parser.add_argument("--csv_out", type=str, default=None, required=False)
    args = parser.parse_args()
    physical_arg = args.physical
    run_builds = args.run_builds
    pd_build_dir = args.build_dir
    collect_data = args.collect_data
    collect_override = args.collect_override
    collect_override_path = pd_build_dir
    collect_data_csv_path = args.csv_out

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
            area_report = os.path.join(collect_override_path, "signoff.area.rpt")
            area_dict = get_area_breakdown_file(file_path=area_report)

        else:
            print(f"Data collection enabled at build dir {pd_build_dir}...")
            all_breakdowns = get_area_breakdown_dir(pd_build_dir)
            # Now emit this information to excel
            write_area_csv(all_breakdowns, collect_data_csv_path)
        exit()


    create_curr_dir = os.path.dirname(os.path.abspath(__file__))

    all_procs = []

    for freq in [1000]:

        for filename in os.listdir(test_files_dir):
            filename_no_ext = os.path.splitext(filename)[0]
            # if filename_no_ext not in ["dual_port_rv", "simple_dual_port"]:
                # continue
            filename_no_ext_f = f"{filename_no_ext}_{freq}"

            head_folder = os.path.join(pd_files_dir, filename_no_ext_f)
            subprocess.run(["rm", "-rf", head_folder])
            subprocess.run(["mkdir", "-p", head_folder])

            other_folder = os.path.join(pd_build_dir, filename_no_ext_f)
            subprocess.run(["mkdir", "-p", other_folder])

            dimensionality = 6

            # Now go through the different data points
            for storage_capacity in [512, 1024, 2048]:
                for data_width in [16]:
                    for clock_count_width in [64]:

                        params_dict = {
                            "design": filename_no_ext,
                            "frequency": freq,
                            "capacity": storage_capacity,
                            "data_width": data_width,
                            "clock_count_width": clock_count_width,
                            "dimensionality": dimensionality
                        }

                        design_folder = f"storage_cap_{storage_capacity}_data_width_{data_width}_ccw_{clock_count_width}_dim_{dimensionality}"
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
                            rtl_configure.write("\n")
                            rtl_configure.write("commands:\n")
                            rtl_configure.write("\n")
                            rtl_configure.write("  - export CURR=$PWD\n")
                            rtl_configure.write("  - echo $CURR\n")
                            rtl_configure.write("\n")
                            rtl_configure.write("  - export TOP=$PWD\n")
                            rtl_configure.write("\n")
                            rtl_configure.write(f"  - python {os.path.join(create_curr_dir, 'create_all_experiments.py')} --physical --storage_capacity {storage_capacity} --clock_count_width {clock_count_width} --data_width {data_width} --outdir $TOP/TEST/\n")
                            rtl_configure.write("\n")
                            rtl_configure.write("  - cd $CURR\n")
                            rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/lakespec.sv outputs/design.v\n")
                            rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/tb.sv outputs/testbench.sv\n")
                            rtl_configure.write(f"  - cat $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/comp_args.txt $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/PARGS.txt > outputs/design.args\n")
                            rtl_configure.write("\n")
                            rtl_configure.write("  - python set_test_dir.py\n")
                            rtl_configure.write("  - echo $PWD\n")

                        print(f"cd {pd_build_path}; mflowgen run --design {full_design_path}")
                        subprocess.run(["mflowgen", "run", "--design", full_design_path], cwd=pd_build_path)

                        # If the builds should go, start it here...
                        if run_builds is True:
                            print(f"Starting build at {pd_build_path}")
                            # execute_str = ["source", make_script]
                            # execute_str = ["make", "6", "&&", "make", "-t", "6", "&&", "make", "17"]
                            execute_str = "make 6; make -t 6; make 17"
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
