%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(OS_CMD_1000,1000).
-define(START_POD_INTERVAL,50).
-define(START_POD_TRIES,50).
-define(STOP_POD_INTERVAL,50).
-define(STOP_POD_TRIES,50).

-define(GITHUB,"/home/pi/erlang/erlang_embedded_system_1/github").
-define(UPDATE_SERVICE(Service,ServiceInfoRecord),controller_lib:update({service,Service},ServiceInfoRecord)).




%% intermodule 
-export([get_nodes/0,
	 create_pod/2, delete_pod/2,get_pods/0,
	 create_container/3,delete_container/3
	]).
%% External exports

%-compile(export_all).
%% ====================================================================
%% External functions
%% ===================================================================
delete_pod(Node,PodId)->
    % Pod='PodId@Host'
    Result=case rpc:call(Node,inet,gethostname,[],5000) of
	       {ok,Host}->
		   PodStr=PodId++"@"++Host,
		   Pod=list_to_atom(PodStr),
		   rpc:call(Pod,init,stop,[],5000),
		    case check_if_vm_stopped(Pod,?STOP_POD_INTERVAL,?STOP_POD_TRIES,error) of
			error->
			    {error,[couldnt_stop_pod,PodId,?MODULE,?LINE]};
			ok->
			    RmCmd="rm -rf "++PodId,
			    case rpc:call(Node,os,cmd,[RmCmd],5000) of
				[]->
				    {ok,stopped};
				Err ->
				    {error,[unknown_error,Err,?MODULE,?LINE]}
			    end
		    end;
	       {badrpc,Err}->
		   {error,[badrpc,Err,?MODULE,?LINE]};
	       Err ->
		   {error,[unknown_error,Err,?MODULE,?LINE]}
	   end,
    Result.
		       


check_if_vm_stopped(_Vm,_Interval,0,ok)->
    ok;
check_if_vm_stopped(_Vm,_Interval,0,error)->
    error;
check_if_vm_stopped(_Vm,_Interval,_N,ok) ->
    ok;
check_if_vm_stopped(Vm,Interval,N,error) ->
    timer:sleep(Interval),
    case net_adm:ping(Vm) of
	pong->
	    NewResult=error;
	pang->
	    NewResult=ok
    end,
    check_if_vm_stopped(Vm,Interval,N-1,NewResult).


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
create_pod(Node,PodId)->
    Result= case create_pod_dir(Node,PodId) of
		{ok,PodStr}->
		    case start_pod(Node,PodId,PodStr) of
			{ok,Pod}->
			    {ok,Pod};
			 %   Service="pod_controller",  %glurk 
			 %   case create_container(Pod,PodId,"pod_controller") of
			%	{ok,Service}->
			%	    {ok,Pod};
			%	{error,Err}->
			%	    {error,Err}
			 %   end;
			{error,Err}->
			    {error,Err}
		    end;
		{error,Err}->
		    {error,Err}
	    end,
    Result.

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
				   {error,[badrpc,Err,Node,PodId,?MODULE,?LINE]};
			       Err ->
				   {error,[unknown_error,Err,Node,PodId,?MODULE,?LINE]}
			   end;
		       {badrpc,Err}->
			   {error,[badrpc,Err,Node,PodId,?MODULE,?LINE]};
		       Err ->
			   {error,[unknown_error,Err,Node,PodId,?MODULE,?LINE]}
		   end;
	       {badrpc,Err}->
		   {error,[badrpc,Err,Node,PodId,?MODULE,?LINE]};
	       Err ->
		   {error,[unknown_error,Err,Node,PodId,?MODULE,?LINE]}
	   end,
    Result.

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

get_pods()->
    AllNodes=nodes(),
    Result=get_pods(AllNodes,[]),
    Result.
    
get_pods([],Pods)->	   
    Pods;
get_pods([Node|T],Acc) ->	
    [W|_]=string:tokens(atom_to_list(Node),"_"),
    case W of
	"worker"->
	    NewAcc=Acc;
	"controller"->
	    NewAcc=Acc;
	_ ->
	    NewAcc=[Node|Acc]
    end,
    get_pods(T,NewAcc).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

get_nodes()->
    AllNodes=nodes(),
    Result=get_nodes(AllNodes,[]),
    Result.
    
get_nodes([],Nodes)->	   
    Nodes;
get_nodes([Node|T],Acc) ->	
    [W|_]=string:tokens(atom_to_list(Node),"_"),
    case W of
	"worker"->
	    NewAcc=[Node|Acc];
	"controller"->
	    NewAcc=[Node|Acc];
	_ ->
	    NewAcc=Acc
    end,
    get_nodes(T,NewAcc).



%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: unload_service(Service,BoardNode)
%% Description:
%% Returns:ok|{error,Err}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:stop_service_node(Service,WorkerNode)
%% Description:
%% Returns:ok|{error,Err}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:create_worker_node(Service,BoardNode)
%% Description:
%% Returns:{ok,PidService}|{error,Err}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
delete_container(Pod,PodId,Service)->
    Result=case rpc:call(Pod,application,stop,[list_to_atom(Service)],10000) of
	       ok->
		   PathServiceEbin=filename:join([PodId,Service,"ebin"]),
		   case rpc:call(Pod,code,del_path,[PathServiceEbin]) of
		       true->
			   PathServiceDir=filename:join(PodId,Service),
			   case rpc:call(Pod,os,cmd,["rm -rf "++PathServiceDir]) of
			       []->
				   ok;
			       Err ->
				   {error,[undefined_error,Pod,PodId,Service,Err,?MODULE,?LINE]}
			   end;
		       false->
			   {error,[directory_not_found,Pod,PodId,Service,?MODULE,?LINE]};
		       {error,Err}->
			   {error,[Pod,PodId,Service,Err,?MODULE,?LINE]};
		       {badrpc,Err} ->
			   {error,[badrpc,Pod,PodId,Service,Err,?MODULE,?LINE]};
		       Err ->
			   {error,[undefined_error,Pod,PodId,Service,Err,?MODULE,?LINE]}
		   end;
	       {error,{not_started,Err}}->
		   {error,[eexists,Pod,PodId,Service,Err,?MODULE,?LINE]};
	       {badrpc,Err} ->
		   {error,[badrpc,Pod,PodId,Service,Err,?MODULE,?LINE]};
	       Err ->
		   {error,[undefined_error,Pod,PodId,Service,Err,?MODULE,?LINE]}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%%
%% PodId/Service
%%
%%
%% --------------------------------------------------------------------
create_container(Pod,PodId,Service)->
    Result =case is_loaded(Pod,PodId,Service) of
		true->
		    {error,[service_already_loaded,Pod,PodId,Service,?MODULE,?LINE]};
		false ->
		    case clone(Pod,PodId,Service) of
			{error,Err}->
		    {error,Err};
			ok ->
			    case compile(Pod,PodId,Service) of
				{error,Err}->
				    {error,Err};
				ok ->
				    %timer:sleep(10000),
				    case start(Pod,PodId,Service) of
					{error,Err}->
					    {error,Err};
					ok->
					    {ok,Service}
				    end
			    end
		    end
	    end,
    Result.
    

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
is_loaded(Pod,PodId,Service)->
    PathToService=filename:join(PodId,Service),
    Result = case rpc:call(Pod,filelib,is_dir,[PathToService],5000) of
		 true->
		     true;
		 false->
		     false;
		 {badrpc,Err} ->
		     {error,[badrpc,Pod,PodId,Service,Err,?MODULE,?LINE]};
		 Err ->
		     {error,[undefined_error,Pod,PodId,Service,Err,?MODULE,?LINE]}
	     end,
    Result.

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
clone(Pod,PodId,Service)->
    Path=filename:join(?GITHUB,Service),
    %Needs to be changed when using git cloen 
    % 1. git clone https .....
    % 2. mv -r Service PodID
    
    Result=case rpc:call(Pod,os,cmd,["cp -r "++Path++" "++PodId]) of
	       []->
		   ok;
	       {badrpc,Err} ->
		   {error,[badrpc,Pod,PodId,Service,Err,?MODULE,?LINE]};
	       Err->
		   {error,Err}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
compile(Pod,PodId,Service)->
    PathSrc=filename:join([PodId,Service,"src"]),
    PathEbin=filename:join([PodId,Service,"ebin"]),
    %Get erl files that shall be compiled
    Result=case rpc:call(Pod,file,list_dir,[PathSrc]) of
	       {ok,Files}->
		   FilesToCompile=[filename:join(PathSrc,File)||File<-Files,filename:extension(File)==".erl"],
		   % clean up ebin dir
		   case rpc:call(Pod,os,cmd,["rm  "++PathEbin++"/*"]) of
		       []->
			   CompileResult=[{rpc:call(Pod,c,c,[ErlFile,[{outdir,PathEbin}]],5000),ErlFile}||ErlFile<-FilesToCompile],
			   case [{R,File}||{R,File}<-CompileResult,error==R] of
			       []->
				   AppFileSrc=filename:join(PathSrc,Service++".app"),
				   AppFileDest=filename:join(PathEbin,Service++".app"),
				   case rpc:call(Pod,os,cmd,["cp "++AppFileSrc++" "++AppFileDest]) of
				       []->
					   ok;
				       {badrpc,Err} ->
					   {error,[badrpc,Pod,PodId,Service,Err,?MODULE,?LINE]};
				       Err ->
					   {error,[undefined_error,Pod,PodId,Service,Err,?MODULE,?LINE]}
				   end;
			       CompilerErrors->
				   {error,[compiler_error,CompilerErrors,?MODULE,?LINE]}
			   end;
		       {badrpc,Err} ->
			   {error,[badrpc,Pod,PodId,Service,Err,?MODULE,?LINE]};
		       Err ->
			   {error,[undefined_error,Pod,PodId,Service,Err,?MODULE,?LINE]}
		   end;
	       {badrpc,Err} ->
		   {error,[badrpc,Pod,PodId,Service,Err,?MODULE,?LINE]};
	       Err ->
		   {error,[undefined_error,Pod,PodId,Service,Err,?MODULE,?LINE]}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
start(Pod,PodId,Service)->
						% glurk=rpc:call(list_to_atom(PodStr),file,list_dir,[PodId++"/*/* "]),
   % glurk=rpc:call(Pod,file,list_dir,[filename:join([PodId,"*","ebin"])]),
    PathServiceEbin=filename:join([PodId,Service,"ebin"]),
    Result = case rpc:call(Pod,code,add_path,[PathServiceEbin],5000) of
		 true->
		     case rpc:call(Pod,application,start,[list_to_atom(Service)],5000) of
			 ok->
			     ok;
			 {badrpc,Err} ->
			     {error,[badrpc,Pod,PodId,Service,Err,?MODULE,?LINE]};
			 Err->
			     {error,[undefined_error,Pod,PodId,Service,Err,?MODULE,?LINE]}
		     end;
		 {badrpc,Err} ->
		     {error,[badrpc,Pod,PodId,Service,Err,?MODULE,?LINE]};
		 Err ->
		     {error,[undefined_error,Pod,PodId,Service,Err,?MODULE,?LINE]}
	     end,
    Result.
