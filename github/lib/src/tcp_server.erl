%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% 2019-10-15:
%%% io:format dont work
%%% close tcp dont work needed to remove loop(Socket) call
%%% Add SSL
%%% -------------------------------------------------------------------
-module(tcp_server).
  


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define (SERVER_SETUP,[binary,{packet,4},{reuseaddr,true},{active,true}]).
-define (TIMEOUT_TCPSERVER, 100*1000).
-define(KEY_M_OS_CMD,89181808).
-define(KEY_F_OS_CMD,"95594968").
-define(KEY_MSG,'100200273').

%% External exports


-export([start_seq_server/1,
	 start_par_server/1
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
start_seq_server(Port)->
    Pid=spawn(fun()->seq_server(Port) end),
    Pid.
		  
seq_server(Port)->
   Result = case gen_tcp:listen(Port,?SERVER_SETUP) of  
	       {ok, LSock}->
		    spawn(fun()->seq_loop(LSock) end),
		    receive
			{_Pid,terminate}->
			    ok
		    end;
	       Err ->
		   {error,Err}
	    end,
    Result.
seq_loop(LSock)->
    {ok,Socket}=gen_tcp:accept(LSock),
    loop(Socket),
    seq_loop(LSock).

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
start_par_server(Port)->
    Pid=spawn(fun()->par_server(Port) end),
    Pid.
par_server(Port)->
    Result = case gen_tcp:listen(Port,?SERVER_SETUP) of  
		 {ok, LSock}->
		     spawn(fun()-> par_connect(LSock) end),
		     receive
			 {_Pid,terminate}->
			     ok
		     end;
		 Err ->
		     Err
	     end,
    Result.

par_connect(LSock)->
    {ok,Socket}=gen_tcp:accept(LSock),
    spawn(fun()-> par_connect(LSock) end),
    loop(Socket).


%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
loop(Socket)->
    receive
	{tcp, Socket, Bin} ->
	    case binary_to_term(Bin) of
		{?KEY_MSG,Pod,cast,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A}}->
		    Result=rpc:cast(Pod,os,cmd,A),
		    gen_tcp:send(Socket, term_to_binary({?KEY_MSG,Result})),
		    loop(Socket);
		{?KEY_MSG,Pod,cast,{M,F,A}}->
		    Result=rpc:cast(Pod,M,F,A),
		    gen_tcp:send(Socket, term_to_binary({?KEY_MSG,Result})),
		    loop(Socket);

		{?KEY_MSG,Pod,call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A}}->
		    Result=rpc:call(Pod,os,cmd,A,?TIMEOUT_TCPSERVER),
		    gen_tcp:send(Socket, term_to_binary({?KEY_MSG,Result})),
		    loop(Socket);
		{?KEY_MSG,Pod,call,{M,F,A}}->
		    Result=rpc:call(Pod,M,F,A,?TIMEOUT_TCPSERVER),
	      
		    io:format("~p~n",[{{?KEY_MSG,Pod,call,{M,F,A}},?MODULE,?LINE}]),
		    gen_tcp:send(Socket, term_to_binary({?KEY_MSG,Result})),
		    loop(Socket);
		{tcp_closed, Socket} ->
		    io:format("socket closed ~n"),
		    init:stop(),
		    tcp_closed;
		Err ->
		    io:format("Err ~p~n",[{Err,?MODULE,?LINE}]),
		  %  glurk=Err,
		 %   io:format("error  ~p~n",[{node(),?MODULE,?LINE,Err,inet:socknames(Socket)}]),
		    gen_tcp:send(Socket, term_to_binary(Err)),
		    loop(Socket)
	    end
    end.
