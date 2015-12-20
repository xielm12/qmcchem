#!/bin/bash
# This script is supposed to run in $QMCCHEM_PATH

ninja -C ocaml clean 
if [[ -d src/IRPF90_temp ]]
then
  ninja -C src/IRPF90_temp -t clean 
fi
ninja -t clean

rm -f ocaml/qmcchem ocaml/.ls_md5 ocaml/generated.ninja
rm -f EZFIO/Ocaml/ezfio.ml
cd src
 rm -rf tags irpf90_entities irpf90.make IRPF90_temp IRPF90_man .ls_md5
 cd MAIN
  rm -f qmc qmcchem_info qmc_create_walkers
cd ../..




