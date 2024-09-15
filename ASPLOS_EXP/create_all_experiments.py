import os
import subprocess
import argparse
import time
from lake.utils.util import check_file_exists_and_has_content
import re


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Generating experiments')
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--serial", action="store_true")
    parser.add_argument("--reg_file", action="store_true")
    parser.add_argument("--run_sim", action="store_true")
    parser.add_argument('--storage_capacity', nargs='*', type=int)
    parser.add_argument('--dimensionality', nargs='*', type=int)
    parser.add_argument('--data_width', nargs='*', type=int)
    parser.add_argument('--clock_count_width', nargs='*', type=int)
    parser.add_argument("--outdir", type=str, default=None, required=True)
    parser.add_argument('--fetch_width', nargs='*', type=int)
    parser.add_argument("--design_filter", type=str, default=None, required=False)
    args = parser.parse_args()
    physical_arg = args.physical
    run_sim = args.run_sim
    reg_file = args.reg_file
    exp_base_dir_arg = args.outdir
    design_filter = args.design_filter
    serial_processing = args.serial

    if design_filter is None:
        design_filter = ""

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

    curr_dir = os.getcwd()

    # all test files...
    base_dir = os.path.dirname(os.path.abspath(__file__))
    lake_base_dir = os.path.join(base_dir, "../")
    test_files_dir = os.path.join(lake_base_dir, "tests/test_spec/")

    test_base = os.path.join(lake_base_dir, "TEST")

    all_procs = []

    all_test_files = os.listdir(test_files_dir)

    filtered_files = [f for f in all_test_files if design_filter in f]

    print("All files...")
    print(filtered_files)

    for filename in filtered_files:

        filename_no_ext = os.path.splitext(filename)[0]
        # Now we want to execute each with the physical flags and a specific name
        total_path_of_file = os.path.join(test_files_dir, filename)

        exp_base_dir = os.path.join(exp_base_dir_arg, filename_no_ext)

        print(f"Generating...{total_path_of_file}")
        # Now go through the different data points
        all_test_pts = ((sc, dataw, ccw, dimw, fw) for sc in storage_capacity_use for dataw in data_width_use for ccw in ccw_use for dimw in dimensionalities_use for fw in fetch_width_use)

        for (storage_capacity, data_width, clock_count_width, dimensionality, fw) in all_test_pts:
            outdir = os.path.join(exp_base_dir, f"storage_cap_{storage_capacity}_data_width_{data_width}_ccw_{clock_count_width}_dim_{dimensionality}_fw_{fw}")
            print(f"Generating exp at ... {outdir}")
            execution_str = ["python", f"{total_path_of_file}", "--storage_capacity", f"{storage_capacity}",
                             "--data_width", f"{data_width}",
                             "--clock_count_width", f"{clock_count_width}",
                             "--outdir", f"{outdir}",
                             "--dimensionality", f"{dimensionality}",
                             "--fetch_width", f"{fw}"]

            if physical_arg:
                execution_str.append("--physical")

            if reg_file:
                execution_str

            vlog_filepath = os.path.join(outdir, "inputs", "lakespec.sv")
            if serial_processing is True:
                result = subprocess.run(execution_str, capture_output=True, text=True)
                print(result.stdout)
            else:
                newp = subprocess.Popen(execution_str, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            # newp = subprocess.Popen(execution_str)
                all_procs.append((newp, vlog_filepath))

            if run_sim:

                newp.wait()
                # Check if output verilog is there...

                assert check_file_exists_and_has_content(vlog_filepath), f"Verilog file at {vlog_filepath} was not created..."

                print(f"Running sim at...{outdir}")
                os.chdir(outdir)
                result = subprocess.run(["make", "sim"], capture_output=True, text=True)
                outdir_basename = os.path.basename(outdir)
                if "PASS" in result.stdout:
                    print(f"Test {outdir_basename} PASS")
                    print(f":)")
                else:
                    print(f"Test {outdir_basename} FAIL")
                    print(f":(")
                os.chdir(curr_dir)

            # else:
            #     # Check if output verilog is there...
            #     vlog_file = os.path.join(outdir, "inputs", "lakespec.sv")
            #     assert check_file_exists_and_has_content(vlog_file), f"Verilog file at {vlog_file} was not created..."

    if serial_processing is False:
        # Wait for all to be done.
        done = False
        while not done:
            done = True
            num_procs_alive = 0
            for proc_, vlfp in all_procs:
                # Still an alive process...
                if proc_.poll() is None:
                    num_procs_alive += 1
            if num_procs_alive > 0:
                print(f"{num_procs_alive} processes still running...")
                time.sleep(3)
                done = False

        print("Done generating all tests...")
        print("Now checking for output verilog...")

        for proc_, vlfp in all_procs:
            assert check_file_exists_and_has_content(vlfp) is True, f"Verilog file at {vlfp} was not created..."
            assert proc_.returncode == 0, f"Proc returned bad value..."
        print("All test collateral verified!")
