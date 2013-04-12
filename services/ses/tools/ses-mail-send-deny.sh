#!/bin/bash

HOST=localhost
SESSION=qwer

./call-ses-api.pl http://$HOST:8888/mail/send $SESSION from:foo@example.com to:bar@example.com data:Hello,World

