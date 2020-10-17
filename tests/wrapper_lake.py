import tempfile

from lake.utils.util import generate_lake_config_wrapper
from lake.utils.util import check_env
from test_lake import base_lake


def get_lake_wrapper(config_path,
                     in_file_name="input",
                     out_file_name="output",
                     in_ports=2,
                     out_ports=2):

    lt_dut, configs, configs_list, magma_dut, tester = \
        base_lake(config_path,
                  in_file_name,
                  out_file_name,
                  in_ports,
                  out_ports)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               flags=["-Wno-fatal"])

    generate_lake_config_wrapper(configs_list, "configs.sv", "build/LakeTop_W.v")


def wrapper():
    lc, ls = check_env()
    config_path = lc + "conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf"

    get_lake_wrapper(config_path=config_path)


#def main():
if __name__ == "__main__":
    wrapper()
