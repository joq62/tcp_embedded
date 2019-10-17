%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(node_config).
  


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(ETS_NAME,node_config_ets).
%% External exports


-export([init/1,delete/0,
	 wanted_state_nodes/1,wanted_state_services/1,
	 create_ets_list/2,
	 zone/0,zone/1,capability/1
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

wanted_state_nodes(ConfigFile)->
    Result = case file:consult(ConfigFile) of
		 {ok,I}->
		     [{NodeStr,list_to_atom(NodeStr)}||NodeStr<-I];
		 {error,Err}->
		     {error,[Err,ConfigFile,?MODULE,?LINE]}
	     end,
    Result.

wanted_state_services(JoscaDir)->
    
    Result = case file:list_dir(JoscaDir) of
		 {ok,Files}->
		     read_josca(Files,JoscaDir,[]);
		 {error,Err}->
		     {error,[Err,JoscaDir,?MODULE,?LINE]}
	     end,
    Result.

read_josca([],_JoscaDir,WantedStateServices)->
    WantedStateServices;
read_josca([File|T],JoscaDir,Acc)->
    {ok,I}=file:consult(filename:join(JoscaDir,File)),
 %   {application_id,AppId}=lists:keyfind(application_id,1,I),
 %   {vsn,Vsn}=lists:keyfind(vsn,1,I),
    {services,ServiceSpecs}=lists:keyfind(services,1,I),
    ServiceList=[{Service,Num,NodeStr}||{{service,Service},{num_instances,Num},{node_str,NodeStr}}<-ServiceSpecs],
    NewAcc=lists:append(ServiceList,Acc),
    read_josca(T,JoscaDir,NewAcc).
    
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
init(ConfigFile)->
    Result = case file:consult(ConfigFile) of
		 {ok,I}->
		     case rpc:call(node(),?MODULE,create_ets_list,[I,[]]) of
			 {badrpc,Err}->
			      {error,[badrpc,Err,create_ets_list,I,?MODULE,?LINE]};
			 EtsList->
			     ets:new(?ETS_NAME, [set, named_table]),
			     rpc:call(node(),ets,insert,[?ETS_NAME,EtsList])
		     end;
		 {error,Err}->
		     {error,[badrpc,Err,create_ets_listfile_consult,ConfigFile,?MODULE,?LINE]}
	     end,
    Result.


delete()->
    ets:delete(?ETS_NAME).

create_ets_list([],EtsList)->
    EtsList;
create_ets_list([{{node_str,N},{capabilities,C},{zone,Z}}|T],Acc)->
    Caps=[{{Cap,N},Cap,N}||Cap<-C],
    Zone=[{{zone,N},Z,N}],
    NewAcc=lists:append([Caps,Zone,Acc]),
    create_ets_list(T,NewAcc).

zone()->
    Result=case ets:match(?ETS_NAME,{{zone,'$1'},'$2','_'}) of
	       []->
		   {error,[no_zones,?MODULE,?LINE]};
	       Zones->
		   A=[{Node,Zone}||[Node,Zone]<-Zones],
		   {ok,A}
	   end,
    Result.
	       
zone(NodeStr)->
    Result=case ets:match(?ETS_NAME,{{zone,NodeStr},'$2','_'}) of
	       []->
		   {error,[no_zones,?MODULE,?LINE]};
	       [[Zone]]->
		   {ok,Zone}
	   end,
    Result.

capability(Capability)->
    Result=case  ets:match(?ETS_NAME,{{Capability,'$1'},'$2','_'}) of
	       []->
		   {ok,[]};
	       Capabilities->
		   A=[{Node,Capability}||[Node,Capability]<-Capabilities],
		   {ok,A}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
