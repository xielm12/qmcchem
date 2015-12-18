#!/bin/bash -x

TARGET=ezfio

function _install()
{
  cd .. ; QMCCHEM_PATH="$PWD" ; cd -
  set -e
  set -u
  rm -rf "${QMCCHEM_PATH}"/EZFIO
  cd "${BUILD}"/config
  rm -f -- qmc.config properties.config
  touch "${QMCCHEM_PATH}"/ezfio_config/properties.config
  ln -s "${QMCCHEM_PATH}"/ezfio_config/qmc.config qmc.config
  ln -s "${QMCCHEM_PATH}"/ezfio_config/properties.config properties.config
  cd -
  mv "${BUILD}" "${QMCCHEM_PATH}"/EZFIO 
}

source scripts/build.sh
