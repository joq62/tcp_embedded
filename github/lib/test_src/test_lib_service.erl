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

-export([start/0]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:init 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ok=init_test(),
    ok=start_seq_server_test(),
    ok=seq_client_test(),    
    ok=stop_test(),
    ok.
init_test()->
    {ok,Host}=inet:gethostname(),
    PodIdServer=?SERVER_ID++"@"++Host,
    PodServer=list_to_atom(PodIdServer),
    pong=net_adm:ping(PodServer),
    ok=rpc:call(PodServer,application,start,[lib_service]),
    ok.

start_seq_server_test()->
    {ok,Host}=inet:gethostname(),
    PodIdServer=?SERVER_ID++"@"++Host,
    PodServer=list_to_atom(PodIdServer),
    ok=rpc:call(PodServer,lib_service,start_tcp_server,[?PORT_SEQ,sequence]),
    
    ok.
seq_client_test()->
    Date=date(),
    Date=rpc:call(node(),tcp_client,call,[{"localhost",?PORT_SEQ},node(),{erlang,date,[]}],200),
   % {badrpc,_}=rpc:call(node(),tcp_client,call,[{"localhost",?PORT_SEQ},node(),{glurk,date,[]}],1000),
    Date=rpc:call(node(),tcp_client,call,[{"localhost",?PORT_SEQ},node(),{erlang,date,[]}],200),
    ok.

stop_seq_server_test()->
    {ok,Host}=inet:gethostname(),
    PodIdServer=?SERVER_ID++"@"++Host,
    PodServer=list_to_atom(PodIdServer),
    {ok,stopped}=rpc:call(PodServer,lib_service,stop_tcp_server,[?PORT_SEQ]),
    ok.	   

seq_client_2_test()->
    Date=date(),
    Date=rpc:call(node(),tcp_client,call,[{"localhost",?PORT_SEQ},node(),{erlang,date,[]}],200),
    ok.

stop_test()->
    {ok,Host}=inet:gethostname(),
    PodIdServer=?SERVER_ID++"@"++Host,
    PodServer=list_to_atom(PodIdServer),
    ok=rpc:call(PodServer,application,stop,[lib_service]),
    ok=rpc:call(PodServer,application,unload,[lib_service]),
    kill(),
    ok.

kill()->
    init:stop().
