all:
	rm -rf brd_ctrl/ebin/*.beam brd_ctrl/src/*~;
	erlc -o brd_ctrl/ebin brd_ctrl/src/*.erl

test:
	rm -rf brd_ctrl/ebin/*.beam brd_ctrl/src/*~;
	erlc -o brd_ctrl/ebin brd_ctrl/src/*.erl;
	erl -pa brd_ctrl/ebin -s brd_ctrl start
