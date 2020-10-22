from lake.helper_test import conv_3_3_args, gen_test_lake
import pytest


# add more tests with this function by adding args
@pytest.mark.parametrize("args", [conv_3_3_args()])
def test_lake(args):
    gen_test_lake(config_path=args[0],
                  stream_path=args[1],
                  in_file_name=args[2],
                  out_file_name=args[3])


if __name__ == "__main__":
    conv33args = conv_3_3_args()
    test_lake(conv33args)
