#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
	-sname web \
	-pa ./ebin -pa ./include \
	-s make all \
	-eval "application:start(nitroweb)"