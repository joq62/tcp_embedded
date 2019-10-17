%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_adder_service). 
 
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
    ok=application:start(adder_service),
    ok.

t1_test()->
    42=adder_service:add(20,22),
    142=adder:add(120,22),
    ok.



stop_test()->
    ok=application:stop(adder_service),
    ok=application:unload(adder_service),
    kill(),
    ok.
kill()->
    init:stop().
