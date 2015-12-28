#!/bin/bash

if [[ -z ${QMCCHEM_PATH} ]]
then
  echo "Error: qmcchemrc not loaded"
  exit -1
fi

cd ${QMCCHEM_PATH}/ocaml || exit -1

exec ninja -f generated.ninja ${@}
