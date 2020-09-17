#!/bin/bash
set +x
set -e
export LAKE_STREAM="$(python3 -m pip show lake | grep 'Location' | cut -d' ' -f2)/sequences/"
cd / && git clone https://github.com/dillonhuff/clockwork && cd clockwork && git checkout lower_ubuffer
export LAKE_CONTROLLERS="/clockwork/lake_controllers/"
# force color
export PYTEST_ADDOPTS="--color=yes"

cd /lake
python3 -m pycodestyle lake/
python3 -m pycodestyle tests/
pytest -v tests/

set -x
