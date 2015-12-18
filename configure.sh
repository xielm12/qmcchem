#!/bin/bash 

set -e
set -u
cd install
mkdir -p Downloads _build
mkdir -p bin
if [[ ! -x ../bin/ninja ]]
then
  echo "Installing Ninja"
  ./scripts/install_ninja.sh &> _build/ninja.log 
  touch _build/ninja.ok
fi
touch ../{src,ocaml}/ls_md5
exec ../bin/ninja "$@"


