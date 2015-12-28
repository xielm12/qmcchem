#!/bin/bash 

set -e
set -u
QMCCHEM_PATH=$PWD
mkdir -p "${QMCCHEM_PATH}"/bin
cd "${QMCCHEM_PATH}"/install
mkdir -p Downloads _build
# TODO : Check if network is up (ping)
if [[ ! -x "${QMCCHEM_PATH}"/bin/ninja ]]
then
  echo "Installing Ninja"
  ./scripts/install_ninja.sh &> _build/ninja.log 
  if [[ ! -x "${QMCCHEM_PATH}"/bin/ninja ]]
  then
    echo "Installation of Ninja failed"
    exit 1
  fi
  touch _build/ninja.ok
fi
touch "${QMCCHEM_PATH}"/{src,ocaml}/.ls_md5
exec "${QMCCHEM_PATH}"/bin/ninja "$@"

if [[ ! -f make.config ]]
then
  which ifort > /dev/null
  if [[ $? ]] 
  then
    cp make.config.ifort make.config
  else
    cp make.config.gfortran make.config
  fi
fi

echo "====================================================================="
echo "Configuration OK."
echo "Now, source the qmcchemrc file:"
echo ""
echo "source qmcchemrc"
echo ""
echo "====================================================================="

