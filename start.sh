#!/bin/bash                                                                     
set -e
cd `dirname $0`
if [ $1 = 'dbm' ]
then
    echo Starting $1.
    exec erl -pa ebin -s make all -sname $1 \
        -mnesia dir "'Mnesia.dbm'" -s mnesia -s crypto
elif [ $1 = 'web' ]
then
    echo Starting $1.
    platform=`uname`
    if [ $platform == 'Darwin' ]
    then
        exec ~/proj/sandbox/nitrogen.git/rel/nitrogen/bin/nitrogen console
    elif [ $platform == 'Linux' ]
    then
        exec ~/pkg/nitrogen.git/rel/nitrogen/bin/nitrogen console
    else
        echo 'unsupported platform: must be one of "Darwin" or "Linux"'
    fi
elif [ $1 = 'lookup' ] || [ $1 = 'rest' ]
then
    echo Starting $1.
    exec erl -pa ebin -s make all -sname $1 -s $1 \
        -mnesia extra_db_nodes "[dbm@`hostname -s`]" \
        -boot start_sasl
else
    echo 'unsupported option: must be one of "dbm", "web", "lookup", or "rest"'
fi
