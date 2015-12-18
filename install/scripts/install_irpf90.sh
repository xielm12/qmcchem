#!/bin/bash -x

TARGET=irpf90
function _install()
{
  set -e
  set -u
  make -C "${BUILD}" -j
  rm -rf -- ../irpf90 
  mv "${BUILD}" ../
  # Check the build is OK
  [[ -x ../irpf90/bin/irpf90 ]]
  [[ -x ../irpf90/bin/irpman ]]
  for i in irpf90 irpman
  do
    rm -rf -- ../bin/$i
    cat << EOF > ../bin/$i
#!/bin/bash -u
exec "\${QMCCHEM_PATH}"/irpf90/bin/$i "\$@"
EOF
    chmod +x ../bin/$i
  done
  return 0
}

source scripts/build.sh


