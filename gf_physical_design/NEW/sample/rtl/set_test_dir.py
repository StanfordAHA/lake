import os
import argparse


def set_test_dir(filename, test_dir):

    test_dir_use = os.path.join("lake", "TEST")
    if test_dir is not None:
        test_dir_use = test_dir

    full_test_dir = os.path.join(os.getcwd(), test_dir_use)

    with open(filename, 'r+') as f:
        lines = f.readlines()
        for i in range(len(lines)):
            line = lines[i]
            if "TEST_DIRECTORY = " in line:
                lines[i] = f'        TEST_DIRECTORY = "{full_test_dir}";\n'

        f.seek(0)
        f.truncate()
        f.writelines(lines)


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Generating experiments')
    parser.add_argument("--test_dir", type=str, default=None, required=False)
    args = parser.parse_args()
    test_dir = args.test_dir
    print()
    filename = "outputs/testbench.sv"

    set_test_dir(filename, test_dir)
