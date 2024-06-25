import os
import subprocess

def find_mem_stub_line(filename):
  with open(filename, 'r') as f:
    for line in f:
      if "mem_stub (" in line:
        return line.split()[0]  # Remove "mem_stub (" and return remaining content
  return None


if __name__ == "__main__":
    print(os.getcwd())
    filename = "inputs/design.v"

    result = find_mem_stub_line(filename)
    assert result, "No macro found"

    # Set the environment variable
    os.environ["MACRO_NAME"] = result
    print(result)

    types = ["S1DB", "S1PB", "SDPB", "R2PB"]
    result_type = None

    for type_ in types:
      if type_ in result:
        result_type = type_
        break

    subprocess.run(["bash", "inputs/adk/gen_srams.sh", result, result_type])
