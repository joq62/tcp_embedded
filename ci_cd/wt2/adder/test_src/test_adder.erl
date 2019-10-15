%%% -------------------------------------------------------------------
%%% Author  : uabjle 
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012 
%%% -------------------------------------------------------------------
-module(test_adder).  
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(SERVER_NODE,'adder@asus').
-define(SERVICE,adder).
%% External exports
-define(TEST_FUNS,[init_test,
		   t1_test,t2_test,t3_test,t4_test,t5_test,
		   cleanup_test]).

-export([start/0,ctrl/0,ctrl_start/0,init_test/0,cleanup_test/0,
	t1_test/0,t2_test/0,t3_test/0,t4_test/0,t5_test/0]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:init 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ctrl_start(),
    TestResult=[{Fun,rpc:call(node(),?MODULE,Fun,[],5000)}||Fun<-?TEST_FUNS],
    FailedTest=[{TestCase,{error,Err}}||{TestCase,{error,Err}}<-TestResult],
    case FailedTest of
	[]->
	   % io:format("OK Sucessfull test ~n"),
	    Result=ok;
	FailedTest->
	    Result=FailedTest
	    %io:format("!!!!!!!!! Failed test ~p~n",[FailedTest])
    end,

    do_kill(),
    Result.



%%%%%%%%%%%%%%%%%% Change here   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_test()->
    ok.

cleanup_test()->
    ok.
t1_test()->
    R= get_worker(?SERVICE),
    case R  of
	{ok,Pid}->
	    Result=l_rpc(Pid,1,reply,add,[20,22]),
	    stopped_normal=l_rpc(Pid,2,reply,stop),
	    ok;
	{error,Err} ->
	    Result={error,Err}		
    end,
    if 
	Result==42->
	    ok;
	true ->
	    {error,Result}
    end.

t2_test()->
   case get_worker(?SERVICE) of
	{ok,Pid}->
	   {Pid,{error,unmatched_signal,glurk}}=l_rpc(Pid,glurk),
	   Result={error,unmatched_signal,glurk},
	   stopped_normal=l_rpc(Pid,2,reply,stop),
	   ok;
	{error,Err} ->
	    Result={error,Err}
   end,
    if 
	Result=={error,unmatched_signal,glurk} ->
	    ok;
	true ->
	    {error,Result}
    end.

t3_test()->
    case get_worker(?SERVICE) of
	{ok,Pid}->
	    Result=l_rpc(Pid,1,reply,divi,[20,5]),
	    stopped_normal=l_rpc(Pid,2,reply,stop),  
	    ok;
	{error,Err} ->
	    Result={error,Err}
    end,
    if 
	Result==4.0 ->
	    ok;
	true ->
	    {error,Result}
    end.

t4_test()->
    case get_worker(?SERVICE) of
	{ok,Pid}->
	    {badrpc,_Err}=l_rpc(Pid,1,reply,divi,[20,0]),
	    Result=badrpc,
	    stopped_normal=l_rpc(Pid,2,reply,stop), 
	    ok;
	{error,Err} ->
	    Result={error,Err}
    end,
    if 
	Result==badrpc->
	    ok;
	true ->
	    {error,Result}
    end.

t5_test()->
    case get_worker(?SERVICE) of
	{ok,Pid}->
	    Result=l_rpc(Pid,1,reply,divi2,[20,0],1000),
	    ok;
	{error,Err} ->
	    Result={error,Err}
    end,
    if 
	Result=={error,timeout}->
	    ok;
	true ->
	    {error,Result}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ctrl_start()->
    Pid_ctrl=spawn(?MODULE,ctrl,[]),
    true=register(ctrl,Pid_ctrl).

ctrl()->
    {ok,PidService}=rpc:call(?SERVER_NODE,?SERVICE,start,[]),
    erlang:monitor(process,PidService),
    ctrl_loop(PidService).
ctrl_loop(PidService)->
    receive
	{From,{allocate,_Service}}->
	    {PidWorker,_Node}=rpc:call(?SERVER_NODE,?SERVICE,allocate,[]),
	    _Ref=erlang:monitor(process,PidWorker),
	    From!{ctrl,{pid,PidWorker}},
	    ctrl_loop(PidService);
	{_From,stop}->
	  %  R=rpc:call(?SERVER_NODE,?SERVICE,stop,[]),
	    PidService!{stop};
	   % io:format("stop  ~p~n",[{?MODULE,?LINE,R}]);
	 {'DOWN',_Ref,process,_Pid,normal}->
	    ctrl_loop(PidService);
	 {'DOWN',_Ref,process,_Pid,Err}->
	    io:format("Down process ~p~n",[{?MODULE,?LINE,Err}]),
	    ctrl_loop(PidService);
	Err ->
	    io:format("Err ~p~n",[{?MODULE,?LINE,Err}]),
	    ctrl_loop(PidService)
    end.
    
get_worker(Service)->	    
    ctrl!{self(),{allocate,Service}},
    Res=receive
	    {ctrl,{pid,Pid}}->
		{ok,Pid}
	after 5000 ->
		{error,timeout}
	end,
    Res.
    

do_kill()->
    ctrl!{self(),stop},
    timer:sleep(1000),
    %init:stop(),
    ok.


l_rpc(Pid,Msg)->
    Pid!{self(),Msg},	   
    receive
	R->
	    R
    end.
    
l_rpc(Pid,Id,Reply,Msg)->
    Pid!{self(),{Id,Reply,Msg}},
    R=case Reply of
	  reply->
	      receive
		  {Pid,{Id,Result}}->
		      Result;
		  Err->
		      Err
	      end;
	  noreply->
	      ok
      end,
    R.


l_rpc(Pid,Id,Reply,F,Args,TimeOut)->
    Pid!{self(),{Id,Reply,F,Args}},
    R=case Reply of
	  reply->
	      receive
		  {Pid,{Id,Result}}->
		      Result;
		  Err->
		    Err 
	      after TimeOut->
		      {error,timeout}
	      end;
	  noreply->
	      ok
      end,
    R.
l_rpc(Pid,Id,Reply,F,Args)->
    Pid!{self(),{Id,Reply,F,Args}},
    R=case Reply of
	  reply->
	      receive
		  {Pid,{Id,Result}}->
		      Result;
		  Err->
		    Err 
	      end;
	  noreply->
	      ok
      end,
    R.
