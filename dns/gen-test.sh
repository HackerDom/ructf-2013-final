#!/bin/bash

DIR=out.test
COUNT=5

[ -d $DIR ] || mkdir $DIR
rm $DIR/*
bin/named-conf-test.pl $COUNT $DIR > $DIR/named.conf.local

