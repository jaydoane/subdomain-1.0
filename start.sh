#!/bin/bash                                                                     
set -e
cd `dirname $0`
if [ $1 = 'dbm' ]
then
    echo Starting $1.
    exec erl -pa ebin -s make all -sname $1 \
        -mnesia dir "'Mnesia.dbm'" -s mnesia -s crypto
elif [ $1 = 'web' ] || [ $1 = 'lookup' ] || [ $1 = 'rest' ]
then
    echo Starting $1.
    exec erl -pa ebin -s make all -sname $1 -s $1 \
        -mnesia extra_db_nodes "[dbm@`hostname -s`]" \
        -boot start_sasl
else
    echo 'unsupported option: must be one of "dbm", "web", "lookup", or "rest"'
fi
