all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s adder_service start -sname adder_service

test:
	rm -rf ebin/* src/*~ test_ebin/* test_src/*~;
	erlc -o ebin src/*.erl;
	erlc -o test_ebin test_src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa test_ebin -s test_adder_service test -sname test_adder_service
