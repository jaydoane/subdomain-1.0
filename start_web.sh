#!/bin/sh
cd `dirname $0`

echo Starting web.
erl \
	-sname web \
	-pa ./ebin -pa ./include \
	-s make all \
	-s crypto start \
	-s mnesia start \
	-eval "application:start(web)"