all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	erl -pa ebin -sname w1
compile:
	rm -rf ebin/* src/*~ src/*.beam test_ebin/* test_src/*~ test_src/*.beam;
	erlc -o ebin src/*.erl;
	erlc -o test_ebin test_src/*.erl
server:
	erl -pa ebin -sname adder	
test:
	erl -pa ebin -pa test_ebin -s test_adder start adder@asus -sname test_adder
