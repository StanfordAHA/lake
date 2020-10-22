from lake.helper_test import *
import pytest


@pytest.mark.parameterize("args", [conv_3_3_args()])
def test_lake(args):
    gen_test_lake(config_path=args[0],
                  stream_path=args[1],
                  in_file_name=args[2],
                  out_file_name=args[3])
