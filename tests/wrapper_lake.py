import argparse
from lake.utils.wrapper import error, wrapper
import pytest


@pytest.mark.skip
@pytest.mark.parametrize("stencil_valid", [True, False])
@pytest.mark.parametrize("name", ["LakeWrapper", "LakeConv33"])
def test_wrapper(stencil_valid,
                 name):
    wrapper(conv_3_3_wrapper_path, stencil_valid, name)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='LakeWrapper')
    parser.add_argument("-c",
                        type=str,
                        help="required: csv_file path relative to LAKE_CONTROLLERS environment variable")
    parser.add_argument("-s",
                        type=str,
                        help="optional: True or False indicating whether or not to generate hardware with stencil_valid (default: True)",
                        default="True")
    parser.add_argument("-n",
                        type=str,
                        help="optional: module name for LakeWrapper module (default: LakeWrapper)",
                        default="LakeWrapper")

    args = parser.parse_args()

    usage = "File usage: python wrapper.py [-c / --csv_file] [csv_file path relative to LAKE_CONTROLLERS environment variable]"
    usage += " [-s / --stencil_valid] [True or False indicating whether or not to generate hardware with stencil_valid (default: True)"
    usage += " [-n] [module name for LakeWrapper module (default: LakeWrapper)]"

    if args.s == "False":
        stencil_valid = False
    elif args.s == "True":
        stencil_valid = True
    else:
        print("Invalid option for stencil valid (must be True or False)...defaulting to True")

    if args.c is None:
        error(usage)

    wrapper(args.c, stencil_valid, args.n)

    # Example usage:
    # python tests/wrapper_lake.py -c conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf
