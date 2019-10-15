%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : Controller to home automation 
%%% Controller shall
%%% Created : 15 Sept 2019
%%% -------------------------------------------------------------------
-module(load_controller).  

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
    os:cmd("cp -r /home/pi/erlang/erlang_embedded_system_1/github/josca /home/pi/erlang/erlang_embedded_system_1/test_env/board_ctrl"),
    {ok,Host}=inet:gethostname(),
    Node=list_to_atom(?NODE_CONTROLLER_ID++"@"++Host),
    PodId="pod_controller",
    {ok,Pod}=test_controller:create_pod(Node,PodId),
    {ok,"controller_service"}=test_controller:create_container(Pod,PodId,"controller_service"),
    {ok,"sd_service"}=test_controller:create_container(Pod,PodId,"sd_service"),
    ok.
   


   

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start_pods([],StartResult)->
    StartResult;
start_pods([{NodeIdStr,PodId}|T],Acc)->
    {ok,Host}=inet:gethostname(),
    Node=list_to_atom(NodeIdStr++"@"++Host),
    PodStr=PodId++"@"++Host,
    NewAcc=case create_pod_dir(Node,PodId) of 
	       {ok,PodStr}->
		   case start_pod(Node,PodId,PodStr) of
		       {ok,Pod}->			   
			   [{ok,Pod}|Acc];
		       {error,Err}->
			   [{error,Err}|Acc]
		   end;
	       {error,Err}->
		   [{error,Err}|Acc]
	   end,
    start_pods(T,NewAcc).
    
    

		   

check_if_vm_started(_Vm,_Interval,0,ok)->
    ok;
check_if_vm_started(_Vm,_Interval,0,error)->
    error;
check_if_vm_started(_Vm,_Interval,_N,ok) ->
    ok;
check_if_vm_started(Vm,Interval,N,error) ->
    timer:sleep(Interval),
    case net_adm:ping(Vm) of
	pang->
	    NewResult=error;
	pong ->
	    NewResult=ok
    end,
    check_if_vm_started(Vm,Interval,N-1,NewResult).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start_pod(Node,PodId,PodStr)->
  %  ErlCmd="erl -pa "++"* "++"-sname "++PodStr++" -detached",
%    ErlCmd="erl -pa "++PodId++"/*/* "++"-sname "++PodStr++" -detached",

     ErlCmd="erl "++"-sname "++PodStr++" -detached",
    Result= case rpc:call(Node,os,cmd,[ErlCmd],5000) of
		[]->
		    case check_if_vm_started(list_to_atom(PodStr),?START_POD_INTERVAL,?START_POD_TRIES,error) of
			error->
			    {error,[couldnt_start_pod,PodId,?MODULE,?LINE]};
			ok->
			    {ok,list_to_atom(PodStr)}
		    end;
	        {badrpc,Err}->
		    {error,[badrpc,Err,?MODULE,?LINE]};
		Err ->
		    {error,[unknown_error,Err,?MODULE,?LINE]}
	    end,
    Result.			
create_pod_dir(Node,PodId)->
    % Pod='PodId@Host'
    Result=case rpc:call(Node,inet,gethostname,[],5000) of
	       {ok,Host}->
		   PodStr=PodId++"@"++Host,
		   %Pod=list_to_atom(PodStr),
		   case rpc:call(Node,filelib,is_dir,[PodId],5000) of
		       true->
			   rpc:call(Node,os,cmd,["rm -rf "++PodId],5000),
			   {error,[pod_already_loaded,PodId,?MODULE,?LINE]};
		       false-> 
			   case rpc:call(Node,file,make_dir,[PodId],5000) of
			       ok->
				   {ok,PodStr};
			       {badrpc,Err}->
				   {error,[badrpc,Err,?MODULE,?LINE]};
			       Err ->
				   {error,[unknown_error,Err,?MODULE,?LINE]}
			   end;
		       {badrpc,Err}->
			   {error,[badrpc,Err,?MODULE,?LINE]};
		       Err ->
			   {error,[unknown_error,Err,?MODULE,?LINE]}
		   end;
	       {badrpc,Err}->
		   {error,[badrpc,Err,Node,PodId,?MODULE,?LINE]};
	       Err ->
		   {error,[unknown_error,Err,?MODULE,?LINE]}
	   end,
    Result.
