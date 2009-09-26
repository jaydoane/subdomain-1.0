# start running an application.
#screen -d -m -S dbm -t "dbm" cd /home/jay/subdomain && ./start_dbm

cd `dirname $0`

screen -d -m -S dbm -t "dbm" \
erl -pa ebin -s make all -sname dbm \
    -mnesia dir "'Mnesia.dbm'" -s mnesia -s crypto

sleep 1

screen -d -m -S lookup -t "lookup" \
erl -pa ebin -s make all -sname lookup -s lookup \
    -mnesia extra_db_nodes "[dbm@`hostname -s`]" \
    -boot start_sasl

screen -d -m -S rest -t "rest" \
erl -pa ebin -sname rest -s rest \
	-mnesia extra_db_nodes "[dbm@`hostname -s`]" \
    -boot start_sasl

screen -d -m -S web -t "web" \
erl -pa ebin -s make all -sname web -s web \
	-mnesia extra_db_nodes "[dbm@`hostname -s`]" \
    -boot start_sasl
