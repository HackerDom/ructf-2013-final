#!/bin/bash

DIR=out.game
COUNT=20

[ -d $DIR ] || mkdir $DIR
rm $DIR/*
bin/named-conf-test.pl $COUNT $DIR > $DIR/named.conf.local

