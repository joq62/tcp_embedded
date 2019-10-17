%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : Node server is the interface to the host
%%% Node server executes all applications specific tasks
%%%  
%%% 2015-03-30: 0.0.1: created
%%% 2015-03-21: 0.0.2: Added node.config file function
%%%                    Lägga till att det går att ta bort eller lägga till
%%%                    gen_Server elel processer vid uppgradering 
%%% Created : 7 March 2015
%%% -------------------------------------------------------------------
-module(q_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define (VERSION,'0.1.0').
%-define (OAM_PORT,2010).   
-define (RPC_TIME,30000).
%% --------------------------------------------------------------------
%% External exports
-export([start/0,inc/0,dec/0,length/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {length}).


%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local,?MODULE}, ?MODULE, [],[]).

length()->
    gen_server:call(?MODULE, {length}).
inc()->
    gen_server:call(?MODULE, {inc}).
dec()->
    gen_server:call(?MODULE, {dec}).
    



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
%% --------------------------------------------------------------------
init([]) -> 
    io:format("start ~p~n",[{?MODULE,?LINE}]),
    {ok, #state{length=0}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({length}, _From, State) ->
    Reply=State#state.length,
    {reply, Reply, State};

handle_call({inc}, _From, State) ->
    Length=State#state.length,
    State1=#state{length=Length+1},
    Reply=State1,
    {reply, Reply, State1};

handle_call({dec}, _From, State) ->
    Length=State#state.length,
    case Length of
	0->
	    State1=State;
	_->
	    State1=#state{length=Length-1}
    end,
    Reply=State1,
    {reply, Reply, State1};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request, From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("unmatched cast signal ~p~n",[{?MODULE,Msg}]),
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("Unmatched signal ~p~n",[{?MODULE,Info}]),
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
%%
%% Local functions
%% 
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Func: copy_beams/1
%% Purpose: Copy the list of Beams
%% Returns: ok - Error
%% --------------------------------------------------------------------
