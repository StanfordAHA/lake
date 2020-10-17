import getopt
import sys
import tempfile

from lake.utils.util import generate_lake_config_wrapper
from lake.utils.util import check_env
from test_lake import base_lake_tester


def get_lake_wrapper(config_path,
                     in_file_name="input",
                     out_file_name="output",
                     in_ports=2,
                     out_ports=2):

    lt_dut, configs, configs_list, magma_dut, tester = \
        base_lake_tester(config_path,
                         in_file_name,
                         out_file_name,
                         in_ports,
                         out_ports)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               flags=["-Wno-fatal"])

    generate_lake_config_wrapper(configs_list, "configs.sv", "build/LakeTop_W.v")


def wrapper(config_path_input):
    lc, ls = check_env()
    config_path = lc + config_path_input
    get_lake_wrapper(config_path=config_path)


def error(usage):
    print(usage)
    sys.exit(2)


def main(argv):
    usage = "File usage: python wrapper.py -c [csv_file path relative to LAKE_CONTROLLERS environment variable]"
    try:
        options, remainder = getopt.getopt(argv, 'c:', ["csv_file="])
    except getopt.GetoptError as e:
        error(usage)

    if len(options) != 1:
        error(usage)

    for opt, arg in options:
        if opt in ("-c", "--csv_file"):
            wrapper(arg)
        else:
            print("Invalid command line argument.")
            error(usage)


if __name__ == "__main__":
    main(sys.argv[1:])

    # Example usage:
    # python tests/wrapper_lake.py -c conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf
