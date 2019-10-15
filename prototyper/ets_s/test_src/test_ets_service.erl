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
-include_lib("stdlib/include/ms_transform.hrl").
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
% severity =emergency|alert|critical|error|warning|notice|info|debug

log_test()->
    ok=log_test:start(),
    ok.

sd_test()->
    ok=sd_test:start(),
    ok.

stop_test()->
    kill(),
    ok.
kill()->
    init:stop().
