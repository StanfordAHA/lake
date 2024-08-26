import os
import subprocess
import argparse


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Generating experiments')
    parser.add_argument("--physical", action="store_true")
    parser.add_argument("--run_sim", action="store_true")
    parser.add_argument("--build_dir", type=str, default=None, required=True)
    args = parser.parse_args()
    physical_arg = args.physical
    run_sim = args.run_sim
    pd_build_dir = args.build_dir

    # all test files...
    base_dir = os.path.dirname(os.path.abspath(__file__))
    lake_base_dir = os.path.join(base_dir, "../")
    test_files_dir = os.path.join(lake_base_dir, "tests/test_spec/")
    pd_files_dir = os.path.join(lake_base_dir, "gf_physical_design/NEW/")

    create_curr_dir = os.path.dirname(os.path.abspath(__file__))

    for freq in [500, 1000]:

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

            # Now go through the different data points
            for storage_capacity in [1024, 2048]:
                for data_width in [16]:
                    for clock_count_width in [64]:
                        design_folder = f"storage_cap_{storage_capacity}_data_width_{data_width}_ccw_{clock_count_width}"
                        full_design_path = os.path.join(head_folder, f"{design_folder}_{freq}")

                        subprocess.run(["rm", "-rf", full_design_path])
                        sample_folder = os.path.join(pd_files_dir, "sample")
                        subprocess.run(["cp", "-r", sample_folder, full_design_path])
                        print(f"Made design folder at {full_design_path}")

                        pd_build_path = os.path.join(pd_build_dir, filename_no_ext_f, design_folder)
                        subprocess.run(["mkdir", "-p", pd_build_path])
                        subprocess.run(["cd", pd_build_path])

                        print(f"cd {pd_build_path}; mflowgen run --design {full_design_path}")
                        # subprocess.run(["pushd", pd_build_path], executable="/bin/bash")
                        subprocess.run(["cd", f"{pd_build_path};", "mflowgen", "run", "--design", full_design_path])
                        # subprocess.run(["mflowgen", "run", "--design", full_design_path])
                        # print(f"Made PD build folder at {pd_build_path}")
                        # subprocess.run(["popd"], executable="/bin/bash")

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
                            # rtl_configure.write("  - git clone https://github.com/StanfordAHA/lake/\n")
                            # rtl_configure.write("  - cd lake\n")
                            # rtl_configure.write("  - git checkout new_asplos\n")
                            rtl_configure.write("  - export TOP=$PWD\n")
                            rtl_configure.write("\n")
                            rtl_configure.write(f"  - python {os.path.join(create_curr_dir, 'create_all_experiments.py')} --physical --storage_capacity {storage_capacity} --clock_count_width {clock_count_width} --data_width {data_width}\n")
                            rtl_configure.write("\n")
                            rtl_configure.write("  - cd $CURR\n")
                            rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/lakespec.sv outputs/design.v\n")
                            rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/tb.sv outputs/testbench.sv\n")
                            rtl_configure.write(f"  - cat $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/comp_args.txt $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/PARGS.txt > outputs/design.args\n")
                            rtl_configure.write("\n")
                            rtl_configure.write("  - python set_test_dir.py\n")
                            rtl_configure.write("  - echo $PWD\n")
