#!/bin/bash
set +x
set -e
cd / && git clone https://github.com/dillonhuff/clockwork && cd clockwork && git checkout lower_ubuffer
export LAKE_CONTROLLERS="/clockwork/lake_controllers/"
export LAKE_STREAM="/clockwork/lake_stream"
# force color
export PYTEST_ADDOPTS="--color=yes"

cd /lake
python3 -m pycodestyle lake/
python3 -m pycodestyle tests/
pytest -v tests/

set -x
