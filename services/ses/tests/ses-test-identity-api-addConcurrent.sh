#!/bin/bash
THREADS=10
for i in `seq 1 $THREADS`
do
    echo "Starting $i ... "
    perl ses-test-identity-api.pl add &
done
