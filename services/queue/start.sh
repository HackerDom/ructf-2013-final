#!/bin/bash

MY_NAME="`readlink -f "$0"`"
MY_DIR="`dirname "$MY_NAME"`"
cd "${MY_DIR}"

LIB_DIR="`readlink -f "$MY_DIR"/rust_compiler/lib/`"

while true; do
    LD_LIBRARY_PATH="./rust_compiler/lib/:${LD_LIBRARY_PATH}" ./queue
    sleep 5
done
