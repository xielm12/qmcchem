#!/bin/bash -x

TARGET=zmq
function _install()
{
  LIBVERSION=4
  set +u
  export C_INCLUDE_PATH="${C_INCLUDE_PATH}":./
  set -e
  set -u
  cd "${BUILD}"
  ./configure --without-libsodium
  make -j 8
  cd - 
  rm -f -- ../lib/libzmq.a ../lib/libzmq.so ../lib/libzmq.so.$LIBVERSION
# cp "${BUILD}"/.libs/libzmq.a ../lib/ 
# cp "${BUILD}"/.libs/libzmq.so ../lib/libzmq.so.$LIBVERSION	
  cp "${BUILD}"/src/.libs/libzmq.a ../lib/ 
  cp "${BUILD}"/src/.libs/libzmq.so ../lib/libzmq.so.$LIBVERSION
  cp "${BUILD}"/include/{zmq,zmq_utils}.h ../lib/
  cd ../lib
  ln libzmq.so.$LIBVERSION libzmq.so || cp libzmq.so.$LIBVERSION libzmq.so
  cd -
  return 0
}

source scripts/build.sh

