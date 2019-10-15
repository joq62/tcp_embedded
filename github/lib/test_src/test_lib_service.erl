%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_lib_service). 
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
-define(PORT_PAR,10000).
-define(PORT_SEQ,10001).

-define(SERVER_ID,"test_tcp_server").
%% External exports

-export([]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:init 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
init_test()->
    {ok,Host}=inet:gethostname(),
    PodIdServer=?SERVER_ID++"@"++Host,
    PodServer=list_to_atom(PodIdServer),
    pong=net_adm:ping(PodServer),
    ok=application:start(lib_service),
    ok.

start_seq_server_test()->
    {ok,Host}=inet:gethostname(),
    PodIdServer=?SERVER_ID++"@"++Host,
    PodServer=list_to_atom(PodIdServer),
    rpc:cast(PodServer,tcp_server,start_seq_server,[?PORT_SEQ]),
    ok.
seq_client_test()->
    Date=date(),
    Date=rpc:call(node(),tcp_client,call,[{"localhost",?PORT_SEQ},node(),{erlang,date,[]}],2000),
  %  Date=rpc:call(node(),tcp_client,call,[{"localhost",?PORT_SEQ},node(),{glurk,date,[]}]),
    ok.
		   
stop_test()->
    ok=application:stop(lib_service),
    ok=application:unload(lib_service),
    kill(),
    ok.

kill()->
    init:stop().
