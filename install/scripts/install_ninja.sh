#!/bin/bash -x

set -u
set -e

TARGET=ninja
URL="http://github.com/martine/ninja/archive/v1.5.3.tar.gz"

function _install()
{
  set -e
  set -u
  cd "${BUILD}"
  ./configure.py --bootstrap
  cd -
  mv "${BUILD}/ninja" ../bin/
  return 0 
}

if [[ ! -f "Downloads/${TARGET}.tar.gz" ]]
then
  wget ${URL} -O "Downloads/${TARGET}.tar.gz" 
fi
source scripts/build.sh

