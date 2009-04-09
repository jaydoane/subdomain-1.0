#!/bin/sh
cd `dirname $0`
echo Starting dbm node.
exec erl -pa ebin -s make all -sname dbm \
    -mnesia dir "'Mnesia.dbm'" -s mnesia -s crypto
