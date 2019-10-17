%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description : ssl_lib is a support package when using ssl in communication
%%% 
%%% -------------------------------------------------------------------
-module(ssl_lib).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-define (TIMEOUT_SSLCLIENT, 15*1000).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/5]).
-export([call/3,cast/3,
	 connect/2,disconnect/1,call/2]).


%%
%% API Function
%%

%% --------------------------------------------------------------------
%% Func: start
%% Purpose: 
%% Returns:
%% --------------------------------------------------------------------
start({M,F},Port,CertFile,KeyFile,Type) ->
   % q_server:start(),
    io:format("~p~n",[{?MODULE,?LINE,ssl:start()}]),
    {ok, LSocket} = ssl:listen(Port, [binary,{packet,4},{certfile,CertFile}, {keyfile,KeyFile}, 
				      {reuseaddr, true}, {active, true}]),
    case Type of
	parallell->
	    spawn(fun() -> par_connect({M,F},LSocket) end);
	sequence ->
	    glurk
    end,
% Following statements must be here otherwise the connection is closed???? 
    receive
	never_match-> 
	    io:format(": ~p~n", [{?MODULE,?LINE}])
    end.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Func: start
%% Purpose: 
%% Returns:
%% --------------------------------------------------------------------
par_connect({M,F},LSocket)->
    {ok, Socket} = ssl:transport_accept(LSocket),
    ok= ssl:ssl_accept(Socket),
    spawn(fun() -> par_connect({M,F},LSocket)	end),
   % q_server:inc(),
    loop({M,F},Socket).

loop({M,F},Socket) ->
%    io:format("loop: ~p~n", [{?MODULE,?LINE,Socket}]),
    ssl:setopts(Socket, [{active, once}]),
    receive
	{ssl,Sock,Bin} ->
	%    io:format("Got packet: ~p~n", [binary_to_term(Bin)]),
	    MsgTerm=binary_to_term(Bin),
	    ReplyTerm=rpc:call(node(),erlang,apply,[M,F,[MsgTerm]]),
	    %Reply=apply(M,F,A),
	    ssl:send(Sock, term_to_binary(ReplyTerm)),
	    loop({M,F},Socket);
	{ssl_closed, Sock} ->
	    ok=ssl:close(Socket);
	    %io:format("Closing socket: ~p~n", [Sock]);
	Error ->
	    ok=ssl:close(Socket),
	    io:format("Error on socket: ~p~n", [Error])
    end,
    %q_server:dec(),
    %io:format(" ~p~n", [{?MODULE,?LINE,time(),q_server:length()}]),
    ok.

%% --------------------------------------------------------------------
%% Func: start
%% Purpose: 
%% Returns:
%% --------------------------------------------------------------------
connect(Addr,Port)->
   % io:format(" ~p~n",[{?MODULE,?LINE,Msg}]),
    case ssl:connect(Addr, Port,  [binary,{packet,4}])of
	{ok,Socket}->
	    Reply = {ok,Socket};
	{error,Err} ->
	    Reply={error,{Err,?MODULE,?LINE}}
    end,	
    Reply.  

%% --------------------------------------------------------------------
%% Func: start
%% Purpose: 
%% Returns:
%% --------------------------------------------------------------------
call(Socket,MsgTerm)->
    MsgBin=term_to_binary(MsgTerm),
    ok = ssl:send(Socket,[MsgBin]),
    receive
	{ssl,{sslsocket,_Z1,_Z2},ReplyIoList}->
	    ReplyBin=iolist_to_binary(ReplyIoList),
	    Reply=binary_to_term(ReplyBin);
	{error,Err} ->
	    Reply={error,{Err,?MODULE,?LINE}};
	X->
	    io:format("unmatched signal ~p~n",[{?MODULE,?LINE,X}]),
	    Reply={error,unmatchd_signal,X}
    after ?TIMEOUT_SSLCLIENT ->
	    Reply={error,tcp_timeout}
    end,
    Reply.  

%% --------------------------------------------------------------------
%% Func: start
%% Purpose: 
%% Returns:
%% --------------------------------------------------------------------
disconnect(Socket)->
    ssl:close(Socket).

%% --------------------------------------------------------------------
%% Func: start
%% Purpose: 
%% Returns:
%% --------------------------------------------------------------------
call(Addr,Port,MsgTerm)->
    case ssl:connect(Addr, Port,  [binary,{packet,4}])of
	{ok,Socket}->
	    MsgBin=term_to_binary(MsgTerm),
	    ok = ssl:send(Socket,[MsgBin]),
	    receive
		{ssl,{sslsocket,_Z1,_Z2},ReplyIoList}->
		    ReplyBin=iolist_to_binary(ReplyIoList),
		    Reply=binary_to_term(ReplyBin),
		    ssl:close(Socket);
		{error,Err} ->
		    Reply={error,{Err,?MODULE,?LINE}},
		    ssl:close(Socket);
		X->
		    io:format("unmatched signal ~p~n",[{?MODULE,?LINE,X}]),
		    Reply={error,unmatchd_signal,X},
		    ssl:close(Socket)
	    after ?TIMEOUT_SSLCLIENT ->
		    Reply={error,tcp_timeout},
		    ssl:close(Socket)
	    end;
	{error,Err} ->
	    Reply={error,{Err,?MODULE,?LINE}}
    end,
    Reply.
%% --------------------------------------------------------------------
%% Func: start
%% Purpose: 
%% Returns:
%% --------------------------------------------------------------------
cast(Addr,Port,MsgTerm)->
   % io:format(" ~p~n",[{?MODULE,?LINE,Msg}]),
    case ssl:connect(Addr, Port,  [binary,{packet,4}])of
	{ok,Socket}->
	    MsgBin=term_to_binary(MsgTerm),
	    Reply = ssl:send(Socket,[MsgBin]),
	    ssl:close(Socket);
	{error,Err} ->
	    Reply={error,{Err,?MODULE,?LINE}}
    end,	
    Reply.  
%% --------------------------------------------------------------------
%% Func: start
%% Purpose: 
%% Returns:
%% --------------------------------------------------------------------
