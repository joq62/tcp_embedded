%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_controller_service).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
-define(W1,'worker_1@asus').
-define(W2,'worker_2@asus').
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
    rpc:call(list_to_atom("pod_1@asus"),init,stop,[]),
    rpc:call(?W1,os,cmd,["rm -rf "++"service_x"]),
    pong=net_adm:ping(?W1),
    pong=net_adm:ping(?W2),
    {ok,_Pid}=controller_service:start(),
    ok.

t1_get_nodes()->
    [worker_2@asus,worker_1@asus]=controller_service:get_nodes(),
ok.    

t2_create_pod_test()->
    {ok,'service_x@asus'}=controller_service:create_pod(?W1,"service_x"),
    pong=net_adm:ping('service_x@asus'),
    {error,[pod_already_loaded,"service_x",controller,_]}=controller_service:create_pod(?W1,"service_x"),
    {error,[badrpc,nodedown,controller,_]}=controller_service:create_pod('glurk@asus',"service_x"),
    ok.

t3_get_pods_test()->
    [service_x@asus]=controller_service:get_pods(),
    ok.

t4_delete_pod_test()->
    {ok,stopped}=controller_service:delete_pod(?W1,"service_x"),
    {ok,stopped}=controller_service:delete_pod(?W1,"service_x"),
    pang=net_adm:ping('service_x@asus'),
    {error,[badrpc,nodedown,controller,_LINE]}=controller_service:delete_pod('glurk@asus',"service_x"),
    ok.
t5_get_pods_test()->
    []=controller_service:get_pods(),
    ok.

t6_create_container_test()->
    Pod='pod_1@asus',
    PodId="pod_1",
    Service="adder_service",
    {ok,Pod}=controller_service:create_pod(?W1,PodId),
    pong=net_adm:ping(Pod),
    {ok,Service}=controller_service:create_container(Pod,PodId,Service),
    42=rpc:call(Pod,adder,add,[20,22],5000),
    42=rpc:call(Pod,adder_service,add,[20,22],5000),
    ok.

t61_test()->
      Pod='pod_1@asus',
    42=rpc:call(Pod,adder,add,[20,22],5000),
    42=rpc:call(Pod,adder_service,add,[20,22],5000),
    ok.
    

t7_delete_container_test()->
    Pod='pod_1@asus',
    PodId="pod_1",
    Service="adder_service",
    ok=controller_service:delete_container(Pod,PodId,Service),
    ok.

t77_delete_container_test()->
    PodId="pod_1",
    {ok,stopped}=controller_service:delete_pod(?W1,PodId),
    ok.


stop_test()->
    controller_service:stop(),
    do_kill().
do_kill()->
    init:stop().
