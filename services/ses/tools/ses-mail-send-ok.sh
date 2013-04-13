#!/bin/bash

HOST=localhost
SESSION=qwer

./call-ses-api.pl http://$HOST:8888/mail/send $SESSION from:foo2@bar.com to:bar@example.com subject:Testing message:Hello,World

