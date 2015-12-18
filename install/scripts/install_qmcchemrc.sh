#!/bin/bash

cd ..
cat << EOF > qmcchemrc
# QMC=Chem environment variables
export QMCCHEM_PATH=${PWD}
export PATH="\${QMCCHEM_PATH}/bin:\${PATH}"
export LD_LIBRARY_PATH="\${QMCCHEM_PATH}/lib:\${LD_LIBRARY_PATH}"
export LIBRARY_PATH="\${QMCCHEM_PATH}/lib:\${LIBRARY_PATH}"
export QMCCHEM_MPIRUN="mpirun"
export QMCCHEM_MPIRUN_FLAGS="--bind-to-core"
#export QMCCHEM_NIC=ib0
source \${QMCCHEM_PATH}/irpf90/bin/irpman
#source \${QMCCHEM_PATH}/EZFIO/Bash/ezfio.sh
EOF

cd -
rm -f _build/qmcchemrc.log
exit 0
