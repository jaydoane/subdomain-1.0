#!/bin/sh
cd `dirname $0`
echo Starting lookup.
exec erl -pa ebin -s make all -sname lookup -s lookup \
    -mnesia extra_db_nodes "[dbm@`hostname -s`]" \
    -boot start_sasl
