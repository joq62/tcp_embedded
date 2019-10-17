%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(adder_service). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{controller_pods,sd_pods}).

%% Definitions 
-define(HB_INTERVAL,1*20*1000).
-define(NODE_CONFIG,"node.config").
%% --------------------------------------------------------------------




-export([add/2
	]).

-export([start/0,
	 stop/0,
	 heart_beat/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals



%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------
add(A,B)->
    gen_server:call(?MODULE, {add,A,B},infinity).


%%-----------------------------------------------------------------------
heart_beat(Interval)->
    gen_server:cast(?MODULE, {heart_beat,Interval}).


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
    {ok,Info} = file:consult(?NODE_CONFIG),
    {controller_pods,ControllerPods}=lists:keyfind(controller_pods,1,Info),
    {sd_pods,SdPods}=lists:keyfind(sd_pods,1,Info),
    spawn(fun()->h_beat(?HB_INTERVAL) end),
    {ok, #state{controller_pods=ControllerPods,sd_pods=SdPods}}.   
    
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

handle_call({add,A,B}, _From, State) ->
     Reply=rpc:call(node(),adder,add,[A,B]),
    {reply, Reply, State};


handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({heart_beat,Interval}, State) ->
    R=[rpc:cast(SdPod,sd_service,register_service,[atom_to_list(?MODULE),node()])||SdPod<-State#state.sd_pods],
%    io:format("Dbg ~p~n",[{?MODULE,?LINE,heart_beat,State#state.sd_pods,R}]),
    spawn(fun()->h_beat(Interval) end),      
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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
h_beat(Interval)->
    timer:sleep(Interval),
    rpc:cast(node(),?MODULE,heart_beat,[Interval]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

