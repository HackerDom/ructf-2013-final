#!/bin/bash

HOST=172.16.16.102
SESSION='eyJsYW5ndWFnZSI6InJ1IiwidWlkIjoiNTE2ZmYxOWNmNDM2NTM4YzYyMDAwMDAzIiwibG9naW4iOiJueGx4cWUiLCJsYXN0X25hbWUiOiJCUkFORCIsImZpcnN0X25hbWUiOiJCYWlsZXkifQ==!3c33bdd5219d311679a92ba2cb9646d967cbda27'

if [ -z "$1" ]
then
    echo Usage: `basename $0` id
    exit 1
fi

./call-ses-api.pl http://$HOST:8888 credentials/del $SESSION id:$1

