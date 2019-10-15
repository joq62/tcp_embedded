all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	erl -pa ebin -sname glurk
compile:
	rm -rf ebin/* src/*~ src/*.beam test_ebin/* test_src/*~ test_src/*.beam;
	erlc -o ebin src/*.erl;
	erlc -o test_ebin test_src/*.erl
server:
	erl -pa ebin -sname dbase	
test:
	erl -pa ebin -pa test_ebin -s test_dbase start -sname test_dbase
