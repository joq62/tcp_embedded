%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : Controller to home automation 
%%% Controller shall
%%% Created : 15 Sept 2019
%%% -------------------------------------------------------------------
-module(load_adder).  

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("test_src/test_include.hrl").

-define(GITHUB,"/home/pi/erlang/erlang_embedded_system_1/github").
-define(OS_CMD_1000,1000).
-define(START_POD_INTERVAL,30).
-define(START_POD_TRIES,10).


%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------


%% ====================================================================
%% External functions
%% ====================================================================


-export([start/0,stop/0
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
    {ok,Host}=inet:gethostname(),
    ControllerPod=list_to_atom(?POD_CONTROLLER_ID++"@"++Host),
    PodId="pod_worker1_1",
    Pod=list_to_atom(PodId++"@"++Host),
    {ok,"adder_service"}=rpc:call(ControllerPod,controller_service,create_container,[Pod,PodId,"adder_service"]),
    Result=rpc:call(Pod,adder,add,[20,22]),
    Result.
	


   

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
stop()->
    {ok,Host}=inet:gethostname(),
    ControllerPod=list_to_atom(?POD_CONTROLLER_ID++"@"++Host),
    PodId="pod_worker1_1",
    Pod=list_to_atom(PodId++"@"++Host),
    R=rpc:call(ControllerPod,controller_service,delete_container,[Pod,PodId,"adder_service"]),
    Result={R,rpc:call(Pod,adder,add,[20,22])},
    Result.
	
