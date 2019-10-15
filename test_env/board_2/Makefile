all:
	rm -rf node_controller;	
	cp -r ../../github/node_controller .
	erl -pa node_controller/ebin -pa node_controller/src -s node_controller_service start -sname node_worker2
