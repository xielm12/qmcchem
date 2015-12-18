#!/bin/bash -x
# This script should be included by all other scripts in this directory

set -u  # All variables should be defined
set -e  # The script should exit if something goes wrong

BUILD="_build/${TARGET}"
rm -rf -- "${BUILD}"
mkdir "${BUILD}"
tar -zxf "Downloads/${TARGET}.tar.gz" --strip-components=1 --directory="${BUILD}"
_install 
rm -rf -- "${BUILD}" "${BUILD}.log"
exit 0

