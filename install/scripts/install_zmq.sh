#!/bin/bash -x

TARGET=zmq
function _install()
{
  LIBVERSION=4
  cd .. ; QMCCHEM_PATH="$PWD" ; cd -
  set +u
  export C_INCLUDE_PATH="${C_INCLUDE_PATH}":./
  set -e
  set -u
  cd "${BUILD}"
  ./configure --without-libsodium
  make -j 8
  cd - 
  rm -f -- "${QMCCHEM_PATH}"/lib/libzmq.{a,so,so.$LIBVERSION} 
# cp "${BUILD}"/.libs/libzmq.a "${QMCCHEM_PATH}"/lib/ 
# cp "${BUILD}"/.libs/libzmq.so "${QMCCHEM_PATH}"/lib/libzmq.so.$LIBVERSION	
  cp "${BUILD}"/src/.libs/libzmq.a "${QMCCHEM_PATH}"/lib/ 
  cp "${BUILD}"/src/.libs/libzmq.so "${QMCCHEM_PATH}"/lib/libzmq.so.$LIBVERSION
  cp "${BUILD}"/include/{zmq,zmq_utils}.h "${QMCCHEM_PATH}"/lib/
  cd "${QMCCHEM_PATH}"/lib
  ln libzmq.so.$LIBVERSION libzmq.so || cp libzmq.so.$LIBVERSION libzmq.so
  cd -
  return 0
}

source scripts/build.sh

