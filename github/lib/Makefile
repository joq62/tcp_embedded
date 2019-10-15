all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s node_controller_service start -sname node_controller_service
server:
	rm -rf ebin/* src/*~ ;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -sname test_tcp_server

test:
	rm -rf ebin/* src/*~ test_ebin/* test_src/*~;
	erlc -o ebin src/*.erl;
	erlc -o test_ebin test_src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa test_ebin -s test_lib_service test -sname test_lib_service
