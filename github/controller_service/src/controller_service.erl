%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller_service).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{nodes,wanted_state_nodes,wanted_state_services}).

-define(NODES_CONFIG,"nodes.config").
-define(NODES_SIMPLE_CONFIG,"nodes_simple.config").
-define(JOSCA,"josca").

% {{service,Service},{pid,PidService},{node_board,NB},{node_service,NS}}

	  
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


%% user interface
-export([
	 
	]).

%% intermodule 
-export([get_nodes/0,
	 create_pod/2,delete_pod/2,get_pods/0,
	 create_container/3,delete_container/3,
	 zone/0,zone/1,capability/1,
	 wanted_state_nodes/0,wanted_state_services/0
	]).

-export([start/0,
	 stop/0
	 ]).
%% internal 
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals

%% Gen server function

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%%----------------------------------------------------------------------
wanted_state_nodes()->
    gen_server:call(?MODULE,{wanted_state_nodes},infinity).
wanted_state_services()->
    gen_server:call(?MODULE,{wanted_state_services},infinity).

zone()->
    gen_server:call(?MODULE,{zone},infinity).

zone(Node)->
    gen_server:call(?MODULE,{zone,Node},infinity).

capability(Capability)->
    gen_server:call(?MODULE,{capability,Capability},infinity).
get_nodes()->
    gen_server:call(?MODULE, {get_nodes},infinity).

get_pods()->
    gen_server:call(?MODULE, {get_pods},infinity).
create_pod(Node,PodId)->
    gen_server:call(?MODULE, {create_pod,Node,PodId},infinity).
delete_pod(Node,PodId)->
    gen_server:call(?MODULE, {delete_pod,Node,PodId},infinity).

create_container(Pod,PodId,Service)->
    gen_server:call(?MODULE, {create_container,Pod,PodId,Service},infinity).
delete_container(Pod,PodId,Service)->
    gen_server:call(?MODULE, {delete_container,Pod,PodId,Service},infinity).

%%-----------------------------------------------------------------------


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    true=node_config:init(?NODES_CONFIG),
    WantedStateNodes=node_config:wanted_state_nodes(?NODES_SIMPLE_CONFIG),
    WantedStateServices=node_config:wanted_state_services(?JOSCA),
    io:format("Dbg ~p~n",[{?MODULE, application_started}]),
    {ok, #state{wanted_state_nodes=WantedStateNodes,
	       wanted_state_services=WantedStateServices}}.   
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({wanted_state_nodes}, _From, State) ->
    Reply=rpc:call(node(),node_config,wanted_state_nodes,[?NODES_SIMPLE_CONFIG]),
    Reply=State#state.wanted_state_nodes, 
    {reply, Reply, State};
 
handle_call({wanted_state_services}, _From, State) ->
    Reply=rpc:call(node(),node_config,wanted_state_services,[?JOSCA]), 
    {reply, Reply, State};

%---------------------------------------------------------------
handle_call({zone}, _From, State) ->
    Reply=rpc:call(node(),node_config,zone,[],5000), 
    {reply, Reply, State};

handle_call({zone,Node}, _From, State) ->
    Reply=rpc:call(node(),node_config,zone,[atom_to_list(Node)],5000),
    {reply, Reply, State};

handle_call({capability,Capability}, _From, State) ->
    Reply=case rpc:call(node(),node_config,capability,[Capability],5000) of
	      []->
		  {ok,[]};
	      {ok,Capabilities}->
		  {ok,Capabilities};
	      Err->
		  {error,[Err,?MODULE,?LINE]}
	  end,
    {reply, Reply, State};

%----------------------------------------------------------------------
handle_call({get_nodes}, _From, State) ->
    Reply=rpc:call(node(),controller,get_nodes,[],5000),
    {reply, Reply, State};

handle_call({get_pods}, _From, State) ->
    Reply=rpc:call(node(),controller,get_pods,[],5000),
    {reply, Reply, State};

handle_call({create_pod,Node,PodId}, _From, State) ->
    Reply=rpc:call(node(),controller,create_pod,[Node,PodId],15000),
    {reply, Reply, State};

handle_call({delete_pod,Node,PodId}, _From, State) ->
    Reply=rpc:call(node(),controller,delete_pod,[Node,PodId],15000),
    {reply, Reply, State};

handle_call({create_container,Pod,PodId,Service}, _From, State) ->
    Reply=rpc:call(node(),controller,create_container,[Pod,PodId,Service],15000),
    {reply, Reply, State};

handle_call({delete_container,Pod,PodId,Service}, _From, State) ->
    Reply=rpc:call(node(),controller,delete_container,[Pod,PodId,Service],15000),
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,?LINE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
