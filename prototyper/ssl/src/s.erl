%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to module_org
-module(s).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, client/1]).

%%
%% API Functions
%%
start() ->
    io:format("~p~n",[{?MODULE,?LINE,ssl:start()}]),
    server(1234).

server(Port) ->
    {ok, LSocket} = ssl:listen(Port, [binary,{packet,4},{certfile,"/home/joq/erlang/code_examples/ssl/ebin/certificate.pem"}, {keyfile, "/home/joq/erlang/code_examples/ssl/ebin/key.pem"}, 
				      {reuseaddr, true}, {active, true}]), 

    spawn(fun() -> par_connect(LSocket) end),
% Following statements must be here otherwise the connection is closed???? 
    receive
	X-> 
	    io:format(": ~p~n", [{?MODULE,?LINE,X}])
    end.


par_connect(LSocket)->
    {ok, Socket} = ssl:transport_accept(LSocket),
    ok= ssl:ssl_accept(Socket),
    spawn(fun() -> par_connect(LSocket)	end),
    loop(Socket).

loop(Socket) ->
    io:format("loop: ~p~n", [{?MODULE,?LINE,Socket}]),
    ssl:setopts(Socket, [{active, once}]),
    receive
	{ssl,Sock, Data} ->
            io:format("Got packet: ~p~n", [Data]),
	    %[A]=Data,
	    {M,F,A}=binary_to_term(Data),
	    Reply=apply(M,F,A),
            ssl:send(Sock, term_to_binary(Reply)),
            loop(Socket);
	{ssl_closed, Sock} ->
            io:format("Closing socket: ~p~n", [Sock]);
	Error ->
            io:format("Error on socket: ~p~n", [Error])
    end.



client({M,F,A}) ->
    Z=term_to_binary({os,cmd,["pwd"]}),

  %  io:format("~p~n",[{?MODULE,?LINE,N}]),
    io:format("~p~n",[{?MODULE,?LINE,ssl:start()}]),
    {ok, Socket} = ssl:connect("localhost", 1234,  [binary,{packet,4}]),
    io:format("Client opened socket: ~p~n",[Socket]),
    ok = ssl:send(Socket,[Z]),
    receive
	{ssl,{sslsocket,_Z1,_Z2},Data}->
	    io:format("Client received Binary: ~p~n",[{?MODULE,?LINE,Data}]),
	    W1=iolist_to_binary(Data),
	    io:format("W1: ~p~n",[{?MODULE,?LINE,W1}]),
	    W2=binary_to_term(W1),
	    io:format("Client received: ~p~n",[{?MODULE,?LINE,W2}]),
	    %Value=binary_to_term(Data);
	    Value=Data;
	X->
	    Value=X,
	    io:format("X: ~p~n",[{?MODULE,?LINE,X}])
    after 2000 ->
	    Value=0
    end,
    ssl:close(Socket),
    io:format("Result: ~p~n",[{?MODULE,?LINE,Value}]).
    
  %  binary_to_term([Value]).
