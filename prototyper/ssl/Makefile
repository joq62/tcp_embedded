## set the paths for a default setup

all:
	erlc -o ebin src/*.erl;
	erl -pa ebin -sname lib 

create:
	erlc -o ebin src/*.erl

clean:
	rm -rf */*~ *.beam */*.beam *~ *.dump
test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin *.erl;
	erl -pa ebin -s test_ssl start  -sname test_ssl

server:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin *.erl;	
	erl -pa ebin -s test_ssl  server -sname server_ssl_test
client:
	erlc -o ebin src/*.erl;
	erlc -o ebin *.erl;
	erl -pa ebin -s test_ssl client  -sname client_ssl_test
