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
    ets:match(?ETS_NAME,{{zone,'$1'},'$2','_'}).
zone(NodeStr)->
    ets:match(?ETS_NAME,{{zone,NodeStr},'$2','_'}).

capability(Capability)->
    ets:match(?ETS_NAME,{{Capability,'$1'},'$2','_'}).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
