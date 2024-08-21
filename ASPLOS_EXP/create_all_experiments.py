import os
import subprocess
import argparse
import time


def check_file_exists_and_has_content(file_path):
    # Check if the file exists
    if os.path.exists(file_path):
        # Check if the file is not empty
        if os.path.getsize(file_path) > 0:
            return True
        else:
            return False
    else:
        return False


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Generating experiments')
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--reg_file", action="store_true")
    parser.add_argument("--run_sim", action="store_true")
    parser.add_argument('--storage_capacity', nargs='*', type=int)
    parser.add_argument('--dimensionality', nargs='*', type=int)
    parser.add_argument('--data_width', nargs='*', type=int)
    parser.add_argument('--clock_count_width', nargs='*', type=int)
    args = parser.parse_args()
    physical_arg = args.physical
    run_sim = args.run_sim
    reg_file = args.reg_file

    dimensionalities_use = [6]
    storage_capacity_use = [1024]
    data_width_use = [16]
    ccw_use = [32, 48, 64]

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

    curr_dir = os.getcwd()

    # all test files...
    base_dir = os.path.dirname(os.path.abspath(__file__))
    lake_base_dir = os.path.join(base_dir, "../")
    test_files_dir = os.path.join(lake_base_dir, "tests/test_spec/")

    test_base = os.path.join(lake_base_dir, "TEST")

    all_procs = []

    for filename in os.listdir(test_files_dir):

        filename_no_ext = os.path.splitext(filename)[0]
        # Now we want to execute each with the physical flags and a specific name
        total_path_of_file = os.path.join(test_files_dir, filename)

        exp_base_dir = os.path.join(test_base, filename_no_ext)

        print(f"Generating...{total_path_of_file}")
        # Now go through the different data points
        all_test_pts = ((sc, dataw, ccw, dimw) for sc in storage_capacity_use for dataw in data_width_use for ccw in ccw_use for dimw in dimensionalities_use)

        for (storage_capacity, data_width, clock_count_width, dimensionality) in all_test_pts:
        # for storage_capacity in storage_capacity_use:
        #     for data_width in data_width_use:
        #         # for clock_count_width in [32, 48, 64]:
        #         for clock_count_width in ccw_use:
            outdir = os.path.join(exp_base_dir, f"storage_cap_{storage_capacity}_data_width_{data_width}_ccw_{clock_count_width}_dim_{dimensionality}")
            print(f"Generating exp at...{outdir}")
            execution_str = ["python", f"{total_path_of_file}", "--storage_capacity", f"{storage_capacity}",
                                "--data_width", f"{data_width}",
                                "--clock_count_width", f"{clock_count_width}",
                                "--outdir", f"{outdir}",
                                "--dimensionality", f"{dimensionality}"]

            if physical_arg:
                execution_str.append("--physical")

            if reg_file:
                execution_str

            vlog_filepath = os.path.join(outdir, "inputs", "lakespec.sv")
            # result = subprocess.run(execution_str, capture_output=True, text=True)
            newp = subprocess.Popen(execution_str, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
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
        assert check_file_exists_and_has_content(vlog_filepath), f"Verilog file at {vlog_filepath} was not created..."
    print("All test collateral verified!")
