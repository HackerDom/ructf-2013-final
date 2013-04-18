#!/bin/bash

if [ -z "$1" ]
then
	echo usage: `basename $0` dir
	exit 1
fi

echo " !!!!!!!!!!!!!!!!!!      ATTENTION      !!!!!!!!!!!!!!!!!!"
echo " I am going to OVERWRITE your /etc/bind/named.conf.local !"
read -p " Are you sure? " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
	cp -v $1/*.db              /var/cache/bind
	cp -v $1/named.conf.local  /etc/bind
	rndc reload
fi

