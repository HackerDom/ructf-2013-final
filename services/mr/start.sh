#!/bin/bash

exec /usr/bin/erl -sname mr -setcookie mr_cookie -noshell -pa ./ebin ./deps/*/ebin ./priv/code -s inets -s yaws -s ibrowse > service.log