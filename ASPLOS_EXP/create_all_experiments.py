import os
import subprocess
import argparse


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Generating experiments')
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--run_sim", action="store_true")
    args = parser.parse_args()
    physical_arg = args.physical
    run_sim = args.run_sim

    curr_dir = os.getcwd()

    # all test files...
    base_dir = os.path.dirname(os.path.abspath(__file__))
    lake_base_dir = os.path.join(base_dir, "../")
    test_files_dir = os.path.join(lake_base_dir, "tests/test_spec/")

    test_base = os.path.join(lake_base_dir, "TEST")
    for filename in os.listdir(test_files_dir):
        filename_no_ext = os.path.splitext(filename)[0]
        # Now we want to execute each with the physical flags and a specific name
        total_path_of_file = os.path.join(test_files_dir, filename)

        exp_base_dir = os.path.join(test_base, filename_no_ext)

        print(f"Generating...{total_path_of_file}")

        # Now go through the different data points
        for storage_capacity in [1024, 2048]:
            for data_width in [16]:
                for clock_count_width in [64]:
                    outdir = os.path.join(exp_base_dir, f"storage_cap_{storage_capacity}_data_width_{data_width}_ccw_{clock_count_width}")
                    print(f"Generating exp at...{outdir}")
                    execution_str = ["python", f"{total_path_of_file}", "--storage_capacity", f"{storage_capacity}",
                                     "--data_width", f"{data_width}",
                                     "--clock_count_width", f"{clock_count_width}",
                                     "--outdir", f"{outdir}"]

                    if physical_arg:
                        execution_str.append("--physical")

                    result = subprocess.run(execution_str, capture_output=True, text=True)

                    if run_sim:
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
