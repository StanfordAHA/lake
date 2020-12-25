#!/bin/bash

# detect the current dir
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ROOT="$(dirname ${ROOT})"

# detect if clockwork as been cloned or not
export CLOCKWORK_DIR="${ROOT}/clockwork"

if [ ! -d ${CLOCKWORK_DIR} ]
then
    git clone -b lower_ubuffer https://github.com/dillonhuff/clockwork ${CLOCKWORK_DIR}
fi

export LAKE_CONTROLLERS="${CLOCKWORK_DIR}/lake_controllers"
export LAKE_STREAM="${CLOCKWORK_DIR}/lake_stream/"
