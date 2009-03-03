#!/bin/sh
cd `dirname $0`

echo Starting dbm node.
erl \
	-sname dbm \
	-pa ./ebin -pa ./include \
	-s make all \
    -mnesia dir "'Mnesia.dbm'" \
    -s mnesia start
