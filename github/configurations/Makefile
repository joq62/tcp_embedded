all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	erl -pa ebin -s controller start -sname controller

test:
	rm -rf ebin/* src/*~ ;
	erlc -o ebin src/*.erl;
	erl -pa ebin  -s conf test -sname conf
