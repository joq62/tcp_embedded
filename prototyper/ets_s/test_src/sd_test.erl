%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sd_test).  
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
-include_lib("stdlib/include/ms_transform.hrl").
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
-record(sd,{service_id,node,pod,time_stamp}).
start()->
    ets:new(sd,[bag,{keypos,#sd.application_id},named_table]),
    ok=sd_init_test(),
    ok=sd_match_test(),
    ok=sd_update_test(),
    ok=sd_remove_test(),
    ets:delete(sd),
    ok.


sd_update(ActualTime)->
    Remove=ets:select(sd,ets:fun2ms(fun(N=#sd{time_stamp=Ts}) when Ts<ActualTime ->
				     N end)),
    Remove.

sd_register(ServiceId,Pod)->
    case sd_service(ServiceId,Pod) of
	[]->
	    % send an error to controller that an unknown service has been register 
	  {error,no_entry};
	Instance->
	    % upate entry with new time stamp
	    ok
    end,
    			   
    ok.

sd_service(ServiceId,WantedPod)->
    Result= ets:select(sd,ets:fun2ms(fun(N=#sd{service_id=SId,pod=Pod}) when SId==ServiceId,Pod==WantedPod ->
						 N end)),
    Result.

sd_service(ServiceId)->
    Result=case ets:select(sd,ets:fun2ms(fun(R=#sd{service_id=SId}) when SId==ServiceId ->
					    R end)) of
	       []->
		   {error,[no_service_instances,?MODULE,?LINE,ServiceId]};
	       ListOfInstances ->
		   {ok,[Pod||{sd,_,_,_,Pod,_}<-ListOfInstances]}
	   end,
    Result.



% Service discovery
sd_init_test()->
    ets:insert(sd,[#sd{application_id="app1",service_id="s1",node=node1,pod=pod11,time_stamp=100},
		   #sd{application_id="app1",service_id="s2",node=node1,pod=pod12,time_stamp=120},
		   #sd{application_id="app1",service_id="s1",node=node2,pod=pod21,time_stamp=90},
		   #sd{application_id="app2",service_id="s3",node=node1,pod=pod13,time_stamp=40}]),
    ok.
    
sd_match_test()->
    [{sd,"app1","s1",node1,pod11,100},
     {sd,"app1","s1",node2,pod21,90}]=ets:select(sd,ets:fun2ms(fun(N=#sd{service_id=SId}) when SId=="s1" ->
					    N end)),

    [{sd,"app1","s1",node1,pod11,100},
     {sd,"app1","s2",node1,pod12,120},
     {sd,"app1","s1",node2,pod21,90}]=ets:select(sd,ets:fun2ms(fun(X=#sd{application_id=AId,node=Node}) when AId=="app1" ->
								       X end)),

    [{sd,"app1","s1",node2,pod21,90}]=ets:select(sd,ets:fun2ms(fun(X=#sd{application_id=AId,node=Node}) when AId=="app1", Node/=node1 ->
					    X end)),
    
    {ok,[pod11,pod21]}=sd_service("s1"),
    {ok,[pod12]}=sd_service("s2"),
    {ok,[pod13]}=sd_service("s3"),
    {error,[no_service_instances,_,_,"glurk"]}=sd_service("glurk"),

    [{sd,"app1","s1",node1,pod11,100}]=sd_service("s1",pod11),
    ok.

sd_update_test()->
    [{sd,"app1","s1",node2,pod21,90},
     {sd,"app2","s3",node1,pod13,40}]=sd_update(100), 
    ok.

sd_remove_test()->
    [{sd,"app1","s1",node1,pod11,100},
     {sd,"app1","s2",node1,pod12,120},
     {sd,"app1","s1",node2,pod21,90},
     {sd,"app2","s3",node1,pod13,40}]=ets:select(sd,ets:fun2ms(fun(X=#sd{})-> X end)),
    TimeOut=100,
    ets:select_delete(sd,ets:fun2ms(fun(#sd{time_stamp=TS}) when TS<TimeOut -> true end)),
    [{sd,"app1","s1",node1,pod11,100},
     {sd,"app1","s2",node1,pod12,120}]=ets:select(sd,ets:fun2ms(fun(X=#sd{})-> X end)),
    ok.
    
stop_test()->
    ets:delete(sd),
    kill(),
    ok.
kill()->
    init:stop().
