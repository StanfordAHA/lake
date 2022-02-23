from lake.utils.test_infra import lake_test_app_args, gen_test_lake
import pytest


# add more tests with this function by adding args
@pytest.mark.skip
# @pytest.mark.parametrize("args", [lake_test_app_args("separate")])
def test_lake(args):
    gen_test_lake(config_path=args[0],
                  stream_path=args[1],
                  in_file_name=args[2],
                  out_file_name=args[3])


if __name__ == "__main__":
    # separate accessors conv_3_3
    conv33args = lake_test_app_args("separate")
    test_lake(conv33args)
