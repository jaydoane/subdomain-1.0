#!/bin/sh
cd `dirname $0`

echo Starting lookup.
erl \
	-sname lookup \
	-pa ./ebin -pa ./include \
	-s make all \
	-s mnesia start \
	-eval "application:start(lookup)"