#!/bin/bash
set +x
set -e

cd /lake

source scripts/setenv.sh

# force color
export PYTEST_ADDOPTS="--color=yes"

# Do we really need these?
# echo pip install py, apt-get install verilator
# pip install py
# apt-get update
# apt-get install verilator

echo python3 -m pycodestyle lake/
python3 -m pycodestyle lake/

echo python3 -m pycodestyle tests/
python3 -m pycodestyle tests/

echo pytest -v tests/
pytest -v tests/

set -x
