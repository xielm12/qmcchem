#!/bin/bash -x

TARGET=f77_zmq
function _install()
{
  cd .. ; QMCCHEM_PATH="$PWD" ; cd -
  set +u
  export C_INCLUDE_PATH="${C_INCLUDE_PATH}":../../../lib
  set -e
  set -u
  cd "${BUILD}"
  export ZMQ_H="${QMCCHEM_PATH}"/lib/zmq.h
  cp "${ZMQ_H}" .
  make -j 8
  cd - 
  rm -f -- "${QMCCHEM_PATH}"/src/ZMQ/f77_zmq.h "${QMCCHEM_PATH}"/lib/libf77zmq.a "${QMCCHEM_PATH}"/lib/libf77zmq.so
  cp "${BUILD}"/libf77zmq.{a,so} ../lib/ 
  cp "${BUILD}"/f77_zmq.h ../src/ZMQ/ 
  return 0
}

source scripts/build.sh

