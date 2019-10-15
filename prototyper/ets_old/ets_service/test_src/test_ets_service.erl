%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_ets_service).  
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------

%% External exports

-export([]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:init 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
init_test()->
    ok=application:start(ets_service),
    ok.

t1_test()->
    42=ets_service:add(20,22),
    142=ets_lib:add(120,22),
    ok.



stop_test()->
    ok=application:stop(ets_service),
    ok=application:unload(ets_service),
    kill(),
    ok.
kill()->
    init:stop().
