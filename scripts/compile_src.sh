#!/bin/bash

if [[ -z ${QMCCHEM_PATH} ]]
then
  echo "Error: qmcchemrc not loaded"
  exit -1
fi

cd ${QMCCHEM_PATH}/src/IRPF90_temp || exit -1

exec ninja ${@}
