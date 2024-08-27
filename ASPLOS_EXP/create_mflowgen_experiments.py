import os
import subprocess
import argparse
import time


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Generating experiments')
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--run_builds", action="store_true")
    parser.add_argument("--build_dir", type=str, default=None, required=True)
    args = parser.parse_args()
    physical_arg = args.physical
    run_builds = args.run_builds
    pd_build_dir = args.build_dir

    # all test files...
    base_dir = os.path.dirname(os.path.abspath(__file__))
    lake_base_dir = os.path.join(base_dir, "../")
    test_files_dir = os.path.join(lake_base_dir, "tests/test_spec/")
    pd_files_dir = os.path.join(lake_base_dir, "gf_physical_design/NEW/")
    make_script = os.path.join(lake_base_dir, "gf_physical_design/", "make_all.sh")

    create_curr_dir = os.path.dirname(os.path.abspath(__file__))

    all_procs = []

    for freq in [1000]:

        for filename in os.listdir(test_files_dir):
            filename_no_ext = os.path.splitext(filename)[0]
            if filename_no_ext not in ["dual_port_rv", "simple_dual_port"]:
                continue
            filename_no_ext_f = f"{filename_no_ext}_{freq}"

            head_folder = os.path.join(pd_files_dir, filename_no_ext_f)
            subprocess.run(["rm", "-rf", head_folder])
            subprocess.run(["mkdir", "-p", head_folder])

            other_folder = os.path.join(pd_build_dir, filename_no_ext_f)
            subprocess.run(["mkdir", "-p", other_folder])

            dimensionality = 6

            # Now go through the different data points
            for storage_capacity in [1024, 2048]:
                for data_width in [16]:
                    for clock_count_width in [64]:
                        design_folder = f"storage_cap_{storage_capacity}_data_width_{data_width}_ccw_{clock_count_width}_dim_{dimensionality}"
                        full_design_path = os.path.join(head_folder, f"{design_folder}_{freq}")

                        subprocess.run(["rm", "-rf", full_design_path])
                        sample_folder = os.path.join(pd_files_dir, "sample")
                        subprocess.run(["cp", "-r", sample_folder, full_design_path])
                        print(f"Made design folder at {full_design_path}")

                        pd_build_path = os.path.join(pd_build_dir, filename_no_ext_f, design_folder)
                        subprocess.run(["mkdir", "-p", pd_build_path])
                        subprocess.run(["cd", pd_build_path])

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
                            execute_str = ["make", "6", "&&", "make", "-t", "6", "&&", "make", "17"]
                            newp = subprocess.Popen(execute_str, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, cwd=pd_build_path)
                            all_procs.append(newp)

                        # print(f"Made PD build folder at {pd_build_path}")

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
