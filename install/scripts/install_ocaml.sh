#!/bin/bash -x

set -u
set -e

cd .. ; QMCCHEM_PATH="$PWD" ; cd -
PACKAGES="core cryptokit ocamlfind sexplib" # ppx_sexp_conv"

declare -i i
i=$(gcc -dumpversion | cut -d '.' -f 2)
if [[ i -lt 6 ]]
then
   echo "GCC version $(gcc -dumpversion) too old. GCC >= 4.6 required."
   exit 1
fi

set +u
source "${QMCCHEM_PATH}"/qmcchemrc
set -u
cd Downloads 
chmod +x opam_installer.sh

if [[ -d "${HOME}"/.opam ]]
then
  set +e
  set +u
  source "${HOME}"/.opam/opam-init/init.sh 
  set -e
  set -u
fi

echo N | ./opam_installer.sh "${QMCCHEM_PATH}"/bin/ 
if [[ ! -f "${QMCCHEM_PATH}"/bin/opam ]]
then
   echo "Installation of OPAM failed"
   exit 2
fi
"${QMCCHEM_PATH}"/bin/opam config setup -a --dot-profile "${QMCCHEM_PATH}"/qmcchemrc 
touch "${QMCCHEM_PATH}"/bin/opam

set +u
export LD_LIBRARY_PATH="${QMCCHEM_PATH}/lib:${LD_LIBRARY_PATH}"
export LIBRARY_PATH="${QMCCHEM_PATH}/lib:${LIBRARY_PATH}"
export C_INCLUDE_PATH="${QMCCHEM_PATH}/lib:${C_INCLUDE_PATH}"
set -u
opam install ${PACKAGES} 
rm "${QMCCHEM_PATH}"/install/_build/ocaml.log
exit 0


