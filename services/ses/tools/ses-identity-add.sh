#!/bin/bash

HOST=localhost
SESSION=qwer

if [ -z "$1" ]
then
    echo Usage: `basename $0` email
    exit 1
fi

./call-ses-api.pl http://$HOST:8888/identity/add $SESSION email:$1

