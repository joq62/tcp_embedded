%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{services}).
-record(worker_state,{}).

% {{service,Service},{pid,PidService},{node_board,NB},{node_service,NS}}

	  
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

-export([
	]).

-export([allocate/0,allocate/1
	]).

-export([start/0,
	 stop/0
	 ]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals

%% Gen server function

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------
allocate(Num)->
    gen_server:call(?MODULE, {allocate,Num},infinity).
allocate()->
    gen_server:call(?MODULE, {allocate},infinity).
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
    {ok, #state{services=[]}}.   
    
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
handle_call({allocate}, _From, State) ->
    [{Pid,_Node}]=start_workers(1,[]),
    Reply={Pid,_Node},
    {reply, Reply, State};

handle_call({allocate,_Num}, _From, State) ->
    Reply=ok,
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

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)

handle_info({From,{MsgId,reply,load_start,[Service,BoardNode]}}, State) ->
    R=rpc:call(node(),controller_lib,load_start,[Service,BoardNode]),
    From!{self(),{MsgId,R}},
    {noreply, State};

handle_info({From,{MsgId,reply,stop_unload,[PidService,Service,WorkerNode,BoardNode]}}, State) ->
    R=rpc:call(node(),controller_lib,stop_unload,[PidService,Service,WorkerNode,BoardNode]),
    From!{self(),{MsgId,R}},
    {noreply, State};



handle_info({From,{MsgId,reply,ping}}, State) ->
    From!{self(),{MsgId,pong}},
    {noreply, State};

handle_info({_From,{glurk,noreply,stop}}, State) ->
    spawn(?MODULE,stop,[]),
    io:format(" ~p~n",[{?MODULE,?LINE,stop}]),
    {noreply, State};

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

start_workers(0,ListOFWorkers)->
    ListOFWorkers;
start_workers(N,Acc) ->
    Worker=init_worker([]),
    NewAcc=[Worker|Acc],
    start_workers(N-1,NewAcc).
%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
init_worker(_Args)->
    State=#worker_state{},
    Pid=spawn(fun()->worker(State) end ),
    {Pid,node()}.

worker(State)->    
    receive
	% Normal cases
	{From,{MsgId,reply,load_start,[application,AppJosca]}}->
	    R=rpc:call(node(),controller_lib,load_start,[application,AppJosca]),
	    From!{self(),{MsgId,R}},
	    NewState=State,
	    worker(NewState);
	{From,{MsgId,reply,load_start,[application,AppJosca],TimeOut}}->
	    R=rpc:call(node(),controller_lib,load_start,[application,AppJosca],TimeOut),
	    From!{self(),{MsgId,R}},
	    NewState=State,
	    worker(NewState);
	
	{From,{MsgId,reply,stop_unload,[application,AppJosca]}}->
	    R=rpc:call(node(),controller_lib,stop_unload,[application,AppJosca]),
	    From!{self(),{MsgId,R}},
	    NewState=State,
	    worker(NewState);
	{From,{MsgId,reply,stop_unload,[application,AppJosca],TimeOut}}->
	    R=rpc:call(node(),controller_lib,stop_unload,[application,AppJosca],TimeOut),
	    From!{self(),{MsgId,R}},
	    NewState=State,
	    worker(NewState);	

	{From,{MsgId,reply,ping}}->
	    From!{self(),{MsgId,pong}},
	    NewState=State,
	    worker(NewState);


	% Control functions mandatory 
	{From,{MsgId,reply,stop}} ->
	    From!{self(),{MsgId,stopped_normal}};
	{From,{MsgId,noreply,stop}} ->
	    From!{self(),{MsgId,stopped_normal}};
	{From,X}->
	    From!{self(),{error,unmatched_signal,X}},
	    NewState=State,
	    worker(NewState);
	Z->
	    io:format("should be error log ~p~n",[{?MODULE,?LINE,Z}]),
	    NewState=State,
	    worker(NewState)
    end.

%% --------------------------------------------------------------------
%% Function: create(Service,PidService,NodeBoard,NodeService)
%% Description: creates an service record 
%% Returns: {ok,Record}||{error,Err}
%% --------------------------------------------------------------------

			
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

