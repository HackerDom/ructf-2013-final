#!/bin/sh

echo "Starting VAPI ..."

perl bin/app.pl --port 3001
#screen -dmS VAPI perl bin/app.pl
#screen -ls | grep VAPI

