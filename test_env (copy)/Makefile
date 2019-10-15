all:
	rm -rf test_ebin/* test_src/*~;
	erlc -o test_ebin test_src/*.erl;
	erl -pa test_ebin -s test_controller start -sname test_controller
compile:
	rm -rf test_ebin/* test_src/*~;
	erlc -o test_ebin test_src/*.erl;
	erl -pa test_ebin -sname test_controller

