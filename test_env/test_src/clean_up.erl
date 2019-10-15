%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : Controller to home automation 
%%% Controller shall
%%% Created : 15 Sept 2019
%%% -------------------------------------------------------------------
-module(clean_up).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("test_src/test_include.hrl").

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
    {ok,Host}=inet:gethostname(),
    NodePodIdList=[{list_to_atom(NodeStr++"@"++Host),PodId}||{NodeStr,PodId}<-?PODS],
    Result=kill_vms(NodePodIdList,[]),
    Result.


   

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
kill_vms([],Result)->
    Result;
kill_vms([{Node,PodId}|T],Acc)->
    NewAcc=[{Node,PodId,test_controller:delete_pod(Node,PodId)}|Acc],
    kill_vms(T,NewAcc).
