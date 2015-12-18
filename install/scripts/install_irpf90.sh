#!/bin/bash -x

TARGET=irpf90
function _install()
{
  cd .. ; QMCCHEM_PATH="$PWD" ; cd -
  set -e
  set -u
  make -C "${BUILD}" -j 8
  rm -rf -- "${QMCCHEM_PATH}"/irpf90 
  mv "${BUILD}" "${QMCCHEM_PATH}"
  # Check the build is OK
  [[ -x "${QMCCHEM_PATH}"/irpf90/bin/irpf90 ]]
  [[ -x "${QMCCHEM_PATH}"/irpf90/bin/irpman ]]
  for i in irpf90 irpman
  do
    rm -rf -- "${QMCCHEM_PATH}"/bin/$i
    cat << EOF > ../bin/$i
#!/bin/bash -u
exec "\${QMCCHEM_PATH}"/irpf90/bin/$i "\$@"
EOF
    chmod +x "${QMCCHEM_PATH}"/bin/$i
  done
  return 0
}

source scripts/build.sh


