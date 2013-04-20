#!/bin/bash
DBNAME=ructf
OUTDIR=/home/user/backup

FNAME=$OUTDIR/`date +"%Y-%m-%d-%H%M%S".sql.gz`
echo -n "Making backup of '$DBNAME' to '$FNAME' ... "
/usr/bin/pg_dump $DBNAME | gzip -c > $FNAME
echo "done: "`stat -c %s $FNAME`" bytes"

