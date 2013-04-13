#!/bin/bash
erl -pa ebin deps/*/ebin priv/code -s inets -s yaws -s ibrowse
