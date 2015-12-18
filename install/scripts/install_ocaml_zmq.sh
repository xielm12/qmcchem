#!/bin/bash -x

cd .. ; QMCCHEM_PATH="$PWD" ; cd -
set -e
set -u


# Check GCC version 
declare -i i
i=$(gcc -dumpversion | cut -d '.' -f 1)
if [[ i -lt 4 ]]
then
   echo "GCC version $(gcc -dumpversion) too old. GCC >= 4.6 required."
   exit 1
fi

if [[ i -eq 4 ]]
then
  i=$(gcc -dumpversion | cut -d '.' -f 2)
  if [[ i -lt 6 ]]
  then
    echo "GCC version $(gcc -dumpversion) too old. GCC >= 4.6 required."
    exit 1
  fi
fi
# End check GCC version 


set +u
source "${QMCCHEM_PATH}"/qmcchemrc
set -u
opam install zmq 
rm -f _build/ocaml_zmq.log
exit 0


