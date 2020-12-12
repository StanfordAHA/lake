#!/bin/bash
set +x
set -e

cd /lake

source scripts/setenv.sh

# force color
export PYTEST_ADDOPTS="--color=yes"

python3 -m pycodestyle lake/
python3 -m pycodestyle tests/
pytest -v tests/

set -x
