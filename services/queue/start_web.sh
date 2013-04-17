#!/bin/bash

MY_NAME="`readlink -f "$0"`"
MY_DIR="`dirname "$MY_NAME"`"
cd "${MY_DIR}"/web/

exec /usr/bin/gunicorn --workers 3 --timeout 3 --bind 0.0.0.0:32550 queue_web:app
