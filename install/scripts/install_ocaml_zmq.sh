#!/bin/bash -x

set -e
set -u

declare -i i
i=$(gcc -dumpversion | cut -d '.' -f 2)
if [[ i -lt 6 ]]
then
   echo "GCC version $(gcc -dumpversion) too old. GCC >= 4.6 required."
   exit 1
fi

set +u
source ../qmcchemrc
set -u
opam install zmq 
rm -f _build/ocaml_zmq.log
exit 0


