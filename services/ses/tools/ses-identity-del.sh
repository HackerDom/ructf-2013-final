#!/bin/bash

HOST=localhost
SESSION=qwer

if [ -z "$1" ]
then
    echo Usage: `basename $0` id
    exit 1
fi

./call-ses-api.pl http://$HOST:8888/identity/del $SESSION id:$1

