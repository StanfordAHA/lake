import os
import subprocess
import argparse
import time
import json
from lake.utils.util import get_file_contents, check_file_exists_and_has_content
import re


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
            print(num_cfg_bits)

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
            if _ in mod_tokens[match_idx] and memoryport_match is False:
                num_matches += 1
                storage_area += float(mod_tokens[3])
        for _ in memintfdec_match:
            if _ in mod_tokens[match_idx]:
                num_matches += 1
                memintf_dec_area += float(mod_tokens[3])

        assert num_matches <= 1, f"Line ({mod}) matched too many items...{num_matches}"

    config_area = total_area - (ag_area + sg_area + id_area + storage_area)

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
    parser.add_argument("--build_dir", type=str, default=None, required=True)
    parser.add_argument("--csv_out", type=str, default=None, required=False)
    parser.add_argument("--design_filter", type=str, default=None, required=False)
    parser.add_argument("--report_path", type=str, default=None, required=False)
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
            print(area_dict)
            print(all_ports_bd)

        else:
            print(f"Data collection enabled at build dir {pd_build_dir}...")
            all_breakdowns = get_area_breakdown_dir(pd_build_dir)
            # Now emit this information to excel
            assert collect_data_csv_path is not None
            write_area_csv(all_breakdowns, collect_data_csv_path)
        exit()

    dimensionalities_use = [6]
    scale_value = 8
    storage_capacity_use = [512 * scale_value, 1024 * scale_value, 2048 * scale_value]
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
