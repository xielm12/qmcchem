#!/bin/bash -x

TARGET=f77_zmq
function _install()
{
  set +u
  export C_INCLUDE_PATH=$C_INCLUDE_PATH:../../../lib
  set -e
  set -u
  cd "${BUILD}"
  export ZMQ_H=../../../lib/zmq.h
  cp "${ZMQ_H}" .
  make -j
  cd - 
  rm -f -- "../src/ZMQ/f77_zmq.h" "../lib/libf77zmq.a" "../lib/libf77zmq.so"
  cp "${BUILD}"/libf77zmq.{a,so} ../lib/ 
  cp "${BUILD}"/f77_zmq.h ../src/ZMQ/ 
  return 0
}

source scripts/build.sh

