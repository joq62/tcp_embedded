%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : Controller to home automation 
%%% Controller shall
%%% Created : 15 Sept 2019
%%% -------------------------------------------------------------------
-module(test_system).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(GITHUB,"/home/pi/erlang/erlang_embedded_system_1/github").
-define(OS_CMD_1000,1000).
%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------


%% ====================================================================
%% External functions
%% ====================================================================


-export([start/0
	 ]).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore             
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
start()->
    R1=clean_up:start(),
    R3=load_controller:start(),
    R4=start_pods:start(),
    R5=load_adder:start(),
    R6=ok,
%    R6=load_adder:stop(),
    io:format("cleanup=~p~nload_controller=~p~nsd_test=~p~nadder_start=~p~nadder_stop=~p~n",[R1,R3,R4,R5,R6]),    
    timer:sleep(10*1000),
    check_sd_controller(),
    
 %   R10=stop_pods:start(),
%    io:format("cleanup=, start_controller_node= ~p~n",[{?MODULE,?LINE,R1,R2}]),
  %  io:format("cleanup=~p~nload_controller=~p~n",[R1,R3]),


  %  receive
%	infinty->
%	    ok
 %   end.
    timer:sleep(1000),
    init:stop(),
    ok.
%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

    
check_sd_controller()->

    {ok,Host}=inet:gethostname(),
    PodId="pod_controller",
    Pod=list_to_atom(PodId++"@"++Host),
    R1=rpc:call(Pod,controller_service,get_nodes,[],5000),
    R2=rpc:call(Pod,sd_service,master_pods,[]),
    R21=rpc:call(Pod,sd_service,master_nodes,[]),
    R3=rpc:call(Pod,sd_service,worker_pods,[]),
    R31=rpc:call(Pod,sd_service,worker_nodes,[]),
    R4=rpc:call(Pod,sd_service,services,[]),
    R41=rpc:call(Pod,application,loaded_applications,[]),
    R5=rpc:call(Pod,controller_service,wanted_state_nodes,[]),
    R51=rpc:call(Pod,controller_service,wanted_state_services,[]),
  %  [["node_worker1@asus",tellstick]]=node_config:capability(tellstick),
   % ok=test_config(Pod),
 %   R10=stop_pods:start(),
%    io:format("cleanup=, start_controller_node= ~p~n",[{?MODULE,?LINE,R1,R2}]),
    io:format("time=~p~nnodes=~p~nmaster_pods=~p~nmaster_nodes=~p~nworker_pods=~p~nworker_nodes=~p~nservices=~p~nloaded_apps=~p~nwanted_state_nodes=~p~nwanted_state_services=~p~n",[time(),R1,R2,R21,R3,R31,R4,R41,R5,R51]),
    timer:sleep(2000),
    check_sd_controller().

test_config(Pod)->
        % zone all
    {ok,[{"node_worker3@asus","varmdeo.main.room2"},
	 {"node_worker1@asus","sthlm.lgh.room1"},
	 {"node_worker2@asus","sthlm.lgh.room2"},
	 {"node_controller1@asus","sthlm.lgh.room2"}]}=rpc:call(Pod,controller_service,zone,[]),
    % zone specific
    {ok,"varmdeo.main.room2"}=rpc:call(Pod,controller_service,zone,['node_worker3@asus']),
    
    % Capability 

    {ok,[{"node_worker1@asus",tellstick},
	 {"node_worker3@asus",tellstick}]}=rpc:call(Pod,controller_service,capability,[tellstick]),
    {ok,[{"node_worker1@asus",disk}]}=rpc:call(Pod,controller_service,capability,[disk]),
    {ok,[]}=rpc:call(Pod,controller_service,capability,[glurk]),
    ok.
