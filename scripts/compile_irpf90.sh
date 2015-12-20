#!/bin/bash 

if [[ -z ${QMCCHEM_PATH} ]]
then
  echo "Error: qmcchemrc not loaded"
  exit -1
fi

cd ${QMCCHEM_PATH}/src

LSMD5_FILE=${QMCCHEM_PATH}/src/.ls_md5
FILES="*.f *.f90 *.F *.py */*.f */*.f90 */*.F */*.py"
MD5=$(ls -ltr --full-time ${FILES} 2>/dev/null | md5sum | cut -d ' ' -f 1)

REF=0

if [[ -f ${LSMD5_FILE} ]]
then
  REF=$(cat ${LSMD5_FILE})
fi

if [[ ${MD5} != ${REF} ]]
then
  echo ${MD5} > ${LSMD5_FILE}
  echo Running IRPF90

  source ${QMCCHEM_PATH}/make.config

  LIB="${LIB} ${QMCCHEM_PATH}/lib/libezfio_irp.a ${QMCCHEM_PATH}/lib/libf77zmq.a ${QMCCHEM_PATH}/lib/libzmq.a -lstdc++ -lrt"
  SRC="${SRC} ZMQ/f77_zmq_module.f90" 
  OBJ="${OBJ} IRPF90_temp/ZMQ/f77_zmq_module.o"
  INCLUDES="${INCLUDES} -I AO -I SAMPLING -I TOOLS -I JASTROW -I TESTING -I MAIN -I PROPERTIES -I ZMQ"
  IRPF90_FLAGS="${IRPF90_FLAGS} --ninja"

  # Check IRPF90 version
  if [[ $( ${IRPF90} -v | python -c "import sys ; print float(sys.stdin.read().rsplit('.',1)[0]) >= 1.6") == False ]]
  then
    echo "IRPF90 version >= 1.6 required"
    exit -1
  fi

  export IRPF90 IRPF90_FLAGS INCLUDES LIB SRC OBJ 

  exec ${IRPF90} ${IRPF90_FLAGS} ${INCLUDES} || exit -1
fi




