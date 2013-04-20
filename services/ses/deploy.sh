#!/bin/bash
cd app
FNAME=../`date +"ses-deploy-%Y%m%d-%H%M.tgz"`
tar czvf $FNAME .
if [ ! -z "$1" ]
then
    echo Uploading to: $1
    scp $FNAME root@$1:
fi
cd ..
