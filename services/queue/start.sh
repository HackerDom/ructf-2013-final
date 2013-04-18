#!/bin/bash

MY_NAME="`readlink -f "$0"`"
MY_DIR="`dirname "$MY_NAME"`"
cd "${MY_DIR}"

LIB_DIR="`readlink -f "$MY_DIR"/rust_compiler/lib/`"

export LD_LIBRARY_PATH="./rust_compiler/lib/:${LD_LIBRARY_PATH}"
export PATH=".:${PATH}"
exec queue > log
