import os

def set_test_dir(filename):

    with open(filename, 'r+') as f:
        lines = f.readlines()
        for i in range(len(lines)):
            line = lines[i]
            if "TEST_DIRECTORY = " in line:
                pwd = os.getcwd()
                lines[i] = f'        TEST_DIRECTORY = "{pwd}/lake/TEST";\n'

        f.seek(0)
        f.truncate()
        f.writelines(lines)

if __name__ == "__main__":
    print()
    filename = "outputs/testbench.sv"

    set_test_dir(filename)
