#!/bin/sh
cd `dirname $0`
echo Starting web.
exec erl -pa ebin -s make all -sname web -s web \
	-mnesia extra_db_nodes "[dbm@`hostname -s`]" \
    -boot start_sasl
