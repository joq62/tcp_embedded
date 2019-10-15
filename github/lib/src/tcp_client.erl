%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(tcp_client).
  


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define (CLIENT_SETUP,[binary, {packet,4}]).
-define (TIMEOUT_TCPCLIENT, 120*1000).

-define(KEY_M_OS_CMD,89181808).
-define(KEY_F_OS_CMD,"95594968").
-define(KEY_MSG,'100200273').

%% External exports


-export([call/3,cast/3
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
call({IpAddr,Port},Pod,{M,F,A})->
    Msg=case {M,F,A} of
	    {os,cmd,A}->
		{?KEY_MSG,Pod,call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A}};
	    {M,F,A}->
		{?KEY_MSG,Pod,call,{M,F,A}}
	end,
    case gen_tcp:connect(IpAddr,Port,?CLIENT_SETUP) of
	{ok,Socket}->
	    ok=gen_tcp:send(Socket,term_to_binary(Msg)),
	    receive
		{tcp,Socket,Bin}->
		    Result=case binary_to_term(Bin) of
			       {?KEY_MSG,R}->
				   R;
			       Err->
				   Err
			   end,
		    gen_tcp:close(Socket);
		{tcp_closed, Socket}->
		    Result={error,tcp_closed}
	    after ?TIMEOUT_TCPCLIENT ->
		    Result={error,[?MODULE,?LINE,tcp_timeout,IpAddr,Port,Msg]},
		    gen_tcp:close(Socket)
	    end;
	{error,Err}->
	    Result={error,Err}
    end,
    Result.
			   
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
cast({IpAddr,Port},Pod,{M,F,A})->
    spawn(fun()->do_cast({IpAddr,Port},Pod,{M,F,A}) end),
    ok.
do_cast({IpAddr,Port},Pod,{M,F,A})->
    Msg=case {M,F,A} of
	    {os,cmd,A}->
		{?KEY_MSG,Pod,cast,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A}};
	    {M,F,A}->
		{?KEY_MSG,Pod,cast,{M,F,A}}
	end,
  case gen_tcp:connect(IpAddr,Port,?CLIENT_SETUP) of
	{ok,Socket}->
	    ok=gen_tcp:send(Socket,term_to_binary(Msg)),
	    receive
		{tcp,Socket,Bin}->
		    Result=case binary_to_term(Bin) of
			       {?KEY_MSG,R}->
				   R;
			       Err->
				   Err
			   end,
		    gen_tcp:close(Socket);
		{tcp_closed, Socket}->
		    Result={error,tcp_closed}
	    after ?TIMEOUT_TCPCLIENT ->
		    Result={error,[?MODULE,?LINE,tcp_timeout,IpAddr,Port,Msg]},
		    gen_tcp:close(Socket)
	    end;
	{error,Err}->
	    Result={error,Err}
    end,
    Result.
