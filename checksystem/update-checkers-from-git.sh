#!/bin/bash

SRC=/home/user/2012/ructfe2012/checkers
DST=/home/user/git/ructf2013-final/checksystem/project/checkers
CUR=`pwd`

echo SRC=$SRC
echo DST=$DST

echo Git pull ...
pushd $SRC
git pull
popd 

echo Copy ...
cp -r $SRC/* $DST

echo Update checkers finished
