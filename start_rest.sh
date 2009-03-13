#!/bin/sh
cd `dirname $0`
exec erl -pa ebin -sname rest -s rest \
	-mnesia extra_db_nodes "[dbm@`hostname -s`]" \
    -boot start_sasl
