#!/bin/bash 

if [[ ! -f qmcchemrc ]]
then
  echo "Error: qmcchemrc not found"
  exit -1
fi

source make.config
source qmcchemrc
FCFLAGS="${FCFLAGS} -fPIC"
export IRPF90 FC FCFLAGS AR RANLIB
cd EZFIO
rm -f make.config
${NINJA} || exit -1
cp lib/libezfio{,_irp}.a ${QMCCHEM_PATH}/lib/ || exit 1


