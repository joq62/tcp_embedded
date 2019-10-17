%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(conf).
  


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
-define(CONFIG_FILE,"nodes.config").
%% External exports


-export([
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

init_test()->
    true=node_config:init(?CONFIG_FILE),
    ok.
 
t1_test()->
        % zone all
    [["node_worker2@asus","sthlm.lgh.room2"],
     ["node_worker3@asus","varmdeo.main.room2"],
     ["node_controller1@asus","sthlm.lgh.room2"],
     ["node_worker1@asus","sthlm.lgh.room1"]]=node_config:zone(),
						% zone specific
    [["varmdeo.main.room2"]]=node_config:zone("node_worker3@asus"),
     % Capability 
    [["node_worker1@asus",tellstick]]=node_config:capability(tellstick),
    [["node_worker1@asus",disk]]=node_config:capability(disk),
    []=node_config:capability(glurk),

    ok.

t2_test()->
    true=node_config:delete(),
    ok.


stop_test()->
    init:stop().
