#!/bin/bash

if [[ -z ${QMCCHEM_PATH} ]]
then
  echo "Error: qmcchemrc not loaded"
  exit -1
fi

cd ${QMCCHEM_PATH}/ocaml

LSMD5_FILE=${QMCCHEM_PATH}/ocaml/.ls_md5
FILES="*.ml *.mli"
MD5=$(ls -ltr --full-time ${FILES} 2>/dev/null | md5sum | cut -d ' ' -f 1)

REF=0

if [[ -f ${LSMD5_FILE} ]]
then
  REF=$(cat ${LSMD5_FILE})
fi

if [[ ${MD5} != ${REF} ]]
then
  echo ${MD5} > ${LSMD5_FILE}
  echo Finding dependencies in OCaml files
  python ./ninja_ocaml.py
fi

ninja ${@}




