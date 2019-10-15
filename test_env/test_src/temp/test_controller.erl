%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : Controller to home automation 
%%% Controller shall
%%% Created : 15 Sept 2019
%%% -------------------------------------------------------------------
-module(test_controller).

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
    prep_test(),
    BoardNodeController='brd_ctrl@asus',
    Brd_worker_1='brd_1@asus',
    Brd_worker_2='brd_2@asus',
    {ok,ControllerPid,ControllerWorkerNode}=load_start("controller",BoardNodeController),
    % start service discovery
    PidSd=spawn(fun()-> sd([]) end),
    register(sd,PidSd),
    
    % load and start adder
    
    {load_start,{ok,PidAdder_1,NodeAdder_1}} = rpc(ControllerPid,{self(),{load_start,reply,load_start,["adder",Brd_worker_1]}},5000),
    io:format("~p~n",[{self(), ?MODULE,?LINE}]), 
    R1=rpc(sd,{self(),{add_service,"adder",PidAdder_1,NodeAdder_1}},1000),
   % io:format("~p~n",[{R1,?MODULE,?LINE}]),    
    {load_start,{ok,PidAdder_2,NodeAdder_2}} = rpc(ControllerPid,{self(),{load_start,reply,load_start,["adder",Brd_worker_2]}},5000),
    R2=rpc(sd,{self(),{add_service,"adder",PidAdder_2,NodeAdder_2}},1000),
   % io:format("~p~n",[{R2,?MODULE,?LINE}]),
    R3=rpc(sd,{self(),{get_service,"adder"}}),
   % io:format("~p~n",[{R3,?MODULE,?LINE}]), 

    PidApp=spawn(fun()-> init_loop("adder",Brd_worker_1,ControllerPid,BoardNodeController,ControllerWorkerNode) end),
    {ok,PidApp}.

sd(ListServices)->
    receive
	{From,{add_service,Service,Pid,Node}}->
	    io:format("~p~n",[{From,add_service,Service,Pid,Node,?MODULE,?LINE}]), 
	    NewList=[{Service,Pid,Node}|lists:delete({Service,Pid,Node},ListServices)],
	    io:format("~p~n",[{NewList, ?MODULE,?LINE}]), 
	    From!{self(),{add_service,NewList}},
	    sd(NewList);
	{From,{remove_service,Service,Pid,Node}}->
	    NewList=lists:delete({Service,Pid,Node},ListServices),
	    From!{self(),{remove_service,NewList}},
	    sd(NewList);
	{From,{get_service,WantedService}}->
	    List=[{Pid,Node}||{Service,Pid,Node}<-ListServices,
			     WantedService==Service],
	    From!{self(),{get_service,List}},
	    NewList=ListServices;
	{From,What} ->
	    From!{self(),{error,What}},
	    NewList=ListServices
    end,
    sd(NewList).


init_loop(Service,BoardService,ControllerPid,BoardNodeController,ControllerWorkerNode)->
    case rpc(sd,{get_service,Service},200) of
	[]->
	    io:format("~p~n error",[{?MODULE,?LINE}]);
	[{PidService,NodeService}|_T]->
	    Ref=monitor(process,PidService),
	    loop(Ref,Service,PidService,NodeService,BoardService,ControllerPid,BoardNodeController,ControllerWorkerNode)
    end.

loop(Ref,Service,PidService,NodeService,BoardService,ControllerPid,BoardNodeController,ControllerWorkerNode)->
    receive
	{From,{add,A,B}}->
	    case rpc(PidService,{self(),{allocate_worker,reply,allocate,[]}},500) of
		{allocate_worker,{PidWorker1,WorkerNode}}->    
		    case rpc(PidWorker1,{self(),{add_req,reply,add,[A,B]}},500) of
			{add_req,Result}->
			    rpc(PidWorker1,{self(),{stop_worker,reply,stop}},500),
			    From!Result;
			Err-> %Worker or communication down
			    io:format("~p~n",[Err]),
			    From!{error,[unknown_error,Err,?MODULE,?LINE]}
		    end;
		Err-> % Service or communication down
		    io:format("~p~n",[Err]),
		    From!{error,[unknown_error,Err,?MODULE,?LINE]}
	    end,
	    loop(Ref,Service,PidService,NodeService,BoardService,ControllerPid,BoardNodeController,ControllerWorkerNode);
	{From,stop}->
	    Result=rpc(ControllerPid,{self(),{stop_unload,reply,stop_unload,[PidService,Service,NodeService,BoardService]}}),
	    From!Result;
	{'DOWN', Ref, Type, Object, Info}->
	    io:format("~p~n",[	{'DOWN', Ref, Type, Object, Info}]),
	    loop(Ref,Service,PidService,NodeService,BoardService,ControllerPid,BoardNodeController,ControllerWorkerNode);
	{From,UnmatchedSignal}->
	    From!{error,[unmatched_signal,UnmatchedSignal,?MODULE,?LINE]},
	    loop(Ref,Service,PidService,NodeService,BoardService,ControllerPid,BoardNodeController,ControllerWorkerNode);
	Err->
	    io:format("~p~n",[Err]),
	    loop(Ref,Service,PidService,NodeService,BoardService,ControllerPid,BoardNodeController,ControllerWorkerNode)
    end.
	


prep_test()->
    BoardNodeController='brd_ctrl@asus',
    Brd_worker_1='brd_1@asus',
    Brd_worker_2='brd_2@asus',    
  
    pong=net_adm:ping(Brd_worker_1),
    pong=net_adm:ping(Brd_worker_2),
    pong=net_adm:ping(BoardNodeController),
    rpc:call('1_controller@asus',init,stop,[]),
    rpc:call('1_adder@asus',init,stop,[]),
    rpc:call('2_adder@asus',init,stop,[]),
    rpc:call('1_divi@asus',init,stop,[]),
    ok.

many_times(_,_,_,0)->
    ok;
many_times(CtrlPid,Service,BoardNode,N)->
    case test_service(CtrlPid,Service,BoardNode) of
	ok->
	    io:format("Success ~p~n",[N]);
	{error,Err}->
	    io:format("~p~n",[{N,Err}])
    end,
    timer:sleep(100), 
    many_times(CtrlPid,Service,BoardNode,N-1).   

    

test_service(CtrlPid,Service,BoardNode)->
    MsgId=self(),
    % Load the Service 
    Result=case rpc(CtrlPid,{self(),{MsgId,reply,load_start,[Service,BoardNode]}}) of
	       {MsgId,{ok,PidService,WorkerNode}}->	
	    % 1 Allocate worker process 
		   {MsgId,{PidWorker1,WorkerNode}}=rpc(PidService,{self(),{self(),reply,allocate,[]}}),
	    %2. Consume the service 
		   case rpc(PidWorker1,{self(),{MsgId,reply,add,[20,22]}}) of
		       {MsgId,42}->
		    % 3. Free the worker process  
			   {MsgId,stopped_normal}=rpc(PidWorker1,{self(),{MsgId,reply,stop}}),
		    % End of normal sequence 
			   {error,_}=rpc(PidWorker1,{self(),{MsgId,reply,add,[20,22]}},500),

		    % Stop and unload the main Service and kill the pod 
			   {MsgId,ok}=rpc(CtrlPid,{self(),{MsgId,reply,stop_unload,[PidService,Service,WorkerNode,BoardNode]}}),
		    % End of complete session 
			   ok;
		       {error,Err}->
			   {error,Err}
		   end;
	       {error,Err}->
		   {error,Err}
	   end,
    Result.



%% --------------------------------------------------------------------
%% Function: unload_service(Service,BoardNode)
%% Description:
%% Returns:ok|{error,Err}
%% --------------------------------------------------------------------
rpc(Pid,Msg)->
    Pid!Msg,
    Result=receive
	       {Pid,Respons}->
		   Respons;
	       Err ->
		   Err
	   end,
    Result.

rpc(Pid,Msg,Timeout)->
    Pid!Msg,
    Result=receive
	       {Pid,Respons}->
		   Respons;
	       Err->
		   Err
	   after Timeout->
		   {error,[timeout,Pid,Msg,?MODULE,?LINE]}
	   end,
    Result.
    
%% --------------------------------------------------------------------
%% Function: unload_service(Service,BoardNode)
%% Description:
%% Returns:ok|{error,Err}
%% --------------------------------------------------------------------







load_start(Service,BoardNode)->
    Result=case check_del_service_dir(Service,BoardNode) of
	       ok->
		   case clone_compile(Service,BoardNode) of
		       ok->
			   case create_worker_node(Service,BoardNode) of
			       {ok,WorkerNode}->
				   case start_service(WorkerNode,Service) of
				       {ok,PidService}->
					   {ok,PidService,WorkerNode};
				       {error,Err} ->
					   {error,Err}
				   end;
			       {error,Err} ->
				   {error,Err}
			   end;
		       {error,Err} ->
			   {error,Err}
		   end;
	       {error,Err} ->
		   {error,Err}
	   end,
    Result.

stop_unload(PidService,Service,WorkerNode,BoardNode)->
    % Stop the service and unload 
    Result=case stop_service_node(PidService,WorkerNode) of
	       ok->
		   unload_service(Service,BoardNode);
	       {error,Err}->
		   {error,Err}
	   end,
    Result.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: unload_service(Service,BoardNode)
%% Description:
%% Returns:ok|{error,Err}
%% --------------------------------------------------------------------
unload_service(Service,BoardNode)->
    rpc:call(BoardNode,os,cmd,["rm -rf "++Service],5000),
    timer:sleep(1000),
    Result = case rpc:call(BoardNode,filelib,is_dir,[Service]) of
		 false->
		     ok;
		 true->
		     {error,[not_deleted_dir,Service,?MODULE,?LINE]};
		 {badrpc,Err}->
		     {error,[badrpc,Err,?MODULE,?LINE]};
		 X ->
		     {error,[unmatched_signal,X,?MODULE,?LINE]}
	     end,
    Result.
%% --------------------------------------------------------------------
%% Function:stop_service_node(Service,WorkerNode)
%% Description:
%% Returns:ok|{error,Err}
%% --------------------------------------------------------------------
stop_service_node(PidService,WorkerNode)->  
    PidService!{self(),{glurk,noreply,stop}},
    Result=case rpc:call(WorkerNode,init,stop,[],5000) of
	       ok->
		   ok;
	       {badrpc,Err}->
		   {error,[badrpc,Err,?MODULE,?LINE]};
	       X ->
		   {error,[unmatched_signal,X,?MODULE,?LINE]}
	   end,
    Result.
%% --------------------------------------------------------------------
%% Function:create_worker_node(Service,BoardNode)
%% Description:
%% Returns:{ok,PidService}|{error,Err}
%% --------------------------------------------------------------------
start_service(WorkerNode,Service)->
    Result=case rpc:call(WorkerNode,list_to_atom(Service),start,[],5000) of
	       {badrpc,Err} ->
		   {error,[badrpc,Err,?MODULE,?LINE]};
	       {ok,PidService}->
		   PidService!{self(),{ping_cmd,reply,ping}},   
		   receive
		       {PidService,{ping_cmd,pong}}->
			   {ok,PidService};
		       Err->
			   {error,[unmatched_signal,Err,?MODULE,?LINE]}
		   after 5000->
			   {error,[timout,PidService,?MODULE,?LINE]}
			       
   
		   end
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:create_worker_node(Service,BoardNode)
%% Description:
%% Returns:{ok,WorkerNode}|{error,Err}
%% --------------------------------------------------------------------
create_worker_node(Service,BoardNode)->
    {ok,WorkerNodeName}={ok,integer_to_list(1)},
    {ok,HostName}=inet:gethostname(),
    UniqueNodeName=WorkerNodeName++"_"++Service,
    WorkerNode=list_to_atom(UniqueNodeName++"@"++HostName),
    Result=case rpc:call(BoardNode,os,cmd,["erl -pa "++Service++"/* "++"-sname "++UniqueNodeName++" -detached"]) of
	       []->
		   timer:sleep(1000),
		   case net_adm:ping(WorkerNode) of
		       pong->
			   {ok,WorkerNode};
		       pang->
			   {error,nodedown}
		   end;
	       {badrpc,nodedown} ->
		   {error,nodedown};
	       Err->
		   {error,Err}
	   end,
    Result.


%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
check_del_service_dir(Service,BoardNode)->
    Result=case rpc:call(BoardNode,filelib,is_dir,[Service],5000) of
	       true->
		   case rpc:call(BoardNode,os,cmd,["rm -rf "++Service]) of
		       []->
			   timer:sleep(?OS_CMD_1000),
			   ok;
		       {badrpc,nodedown} ->
			   {error,nodedown};
		       Err->
			   {error,Err}
		   end;
	       false->
		   ok;
	       {badrpc,nodedown} ->
		   {error,nodedown};
	       Err->
		   {error,Err}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
clone_compile(Service,BoardNode)->
    Result=case clone(Service,BoardNode) of
	       ok->
		   compile_erl(filename:join(Service,"src"),filename:join(Service,"ebin"),BoardNode);
	       {badrpc,nodedown}->
		   {error,nodedown};
	       Err->
		   io:format(" ~p~n",[{?MODULE,?LINE,Err}]),
		   {error,[compiler_error,Err,?MODULE,?LINE]}
	   end,
    Result.
    

clone(Service,BoardNode)->
    Path=filename:join(?GITHUB,Service),
    Result=case rpc:call(BoardNode,os,cmd,["cp -r "++Path++" ."]) of
	       []->
		   ok;
	       {badrpc,nodedown}->
		   {error,nodedown};
	       Err->
		   {error,Err}
	   end,
    Result.

compile_erl(Src,Dest,BoardNode)->
  %  io:format("~p~n",[{?MODULE,?LINE,Src,Dest,BoardNode}]),
    Result=case rpc:call(BoardNode,file,list_dir,[Src]) of
	       {ok,Files}->
		   FilesToCompile=[filename:join(Src,File)||File<-Files,filename:extension(File)==".erl"],
		   case rpc:call(BoardNode,os,cmd,["rm  "++Dest++"/*"]) of
		       []->
			   CompileResult=[{rpc:call(BoardNode,c,c,[ErlFile,[{outdir,Dest}]],5000),ErlFile}||ErlFile<-FilesToCompile],
			   case [{R,File}||{R,File}<-CompileResult,error==R] of
			       []->
				   ok;
			       CompilerErrors->
				   {error,[compiler_error,CompilerErrors,?MODULE,?LINE]}
			   end;
		       {badrpc,nodedown}->
			   {error,nodedown};
		       Err->
			   {error,Err}
		   end;
	       {badrpc,nodedown}->
		   {error,nodedown};
	       Err->
		   {error,Err}
	   end,
    Result.
   

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
  
