from lake.utils.util import verify_gold
import argparse


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Test verifier...')
    parser.add_argument("--dir", type=str, default=None)
    args = parser.parse_args()
    ret_val = verify_gold(args.dir, mflowgen=True)
    if ret_val is True:
        print("Test PASSED!")
    else:
        print("Test FAILED...")
