import os
import subprocess
import argparse


if __name__ == "__main__":

    # all test files...
    base_dir = os.path.dirname(os.path.abspath(__file__))
    lake_base_dir = os.path.join(base_dir, "../")
    test_files_dir = os.path.join(lake_base_dir, "tests/test_spec/")
    pd_files_dir = os.path.join(lake_base_dir, "gf_physical_design/NEW/")
    pd_build_dir = "/sim/skavya/lake/NEW/"

    for filename in os.listdir(test_files_dir):
        filename_no_ext = os.path.splitext(filename)[0]

        head_folder = f"{pd_files_dir}{filename_no_ext}/"

        subprocess.run(["rm", "-rf", f"{head_folder}"])
        subprocess.run(["mkdir", f"{head_folder}"])

        subprocess.run(["mkdir", "-p", f"{pd_build_dir}{filename_no_ext}"])

        # Now go through the different data points
        for storage_capacity in [1024, 2048]:
            for data_width in [16]:
                for clock_count_width in [64]:
                    design_folder = f"storage_cap_{storage_capacity}_data_width_{data_width}_ccw_{clock_count_width}"

                    full_design_path = f"{head_folder}{design_folder}/"
                    
                    subprocess.run(["rm", "-rf", f"{full_design_path}"])
                    subprocess.run(["cp", "-r", f"{pd_files_dir}sample", f"{full_design_path}"])

                    # print(f"Made design folder at {full_design_path}")

                    pd_build_path = f"{pd_build_dir}{filename_no_ext}/{design_folder}"

                    subprocess.run(["mkdir", "-p", f"{pd_build_path}"])
                    subprocess.run(["cd", f"{pd_build_path}"])
                    # subprocess.run(["mflowgen", "run", "--design", f"{full_design_path}"])

                    print(f"cd {pd_build_path}")
                    print(f"mflowgen run --design {full_design_path}")

                    # print(f"Made PD build folder at {pd_build_path}")

                    with open(f"{full_design_path}rtl/configure.yml", "w") as rtl_configure:
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
                        rtl_configure.write("  - git clone https://github.com/StanfordAHA/lake/\n")
                        rtl_configure.write("  - cd lake\n")
                        rtl_configure.write("  - git checkout new_asplos\n")
                        rtl_configure.write("  - export TOP=$PWD\n")
                        rtl_configure.write("\n")
                        rtl_configure.write("  - python3.7 -m venv lake_env\n")
                        rtl_configure.write("  - source lake_env/bin/activate\n")
                        rtl_configure.write("  - python -m pip install --upgrade pip\n")
                        rtl_configure.write("  - pip install -e .\n")
                        rtl_configure.write("\n")
                        rtl_configure.write("  - python ASPLOS_EXP/create_all_experiments.py --physical\n")
                        rtl_configure.write("\n")
                        rtl_configure.write("  - cd $CURR\n")
                        rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/lakespec.sv outputs/design.v\n")
                        rtl_configure.write(f"  - cp $TOP/TEST/{filename_no_ext}/{design_folder}/tb.sv outputs/testbench.sv\n")
                        rtl_configure.write(f"  - cat $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/comp_args.txt $TOP/TEST/{filename_no_ext}/{design_folder}/inputs/PARGS.txt > outputs/design.args\n")
                        rtl_configure.write("\n")
                        rtl_configure.write("  - echo $PWD\n")

                    