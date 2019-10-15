%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(log_test).  
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-include_lib("stdlib/include/ms_transform.hrl").
%% External exports

-export([start/0]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:init 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
-record(log,{date,time,node,module,line,severity,info}).


-define(LOG(Date,Time,Node,Module,Line,Severity,Info),
       ets:insert(log,#log{date=Date,time=Time,
			   node=Node,module=Module,line=Line,
			   severity=Severity,
			   info=Info})).
% severity =emergency|alert|critical|error|warning|notice|info|debug

start()->
    ets:new(log,[bag,{keypos,#log.date},named_table]),
    ok=to_date_time_test(),
    ok=t2_test(),
    ok=t2_match_test(),
    ets:delete(log),
    ok.

to_date_time_test()->
    date_str(date()),
    time_str(time()),
    date_time_str(date(),time()),
    ok.

%% Log test
t2_test()->
    ets:insert(log,[#log{date={2019,01,01},time={00,00,00},
			 node=node1,module=module1,line=line1,
			 severity=emergency,
			 info="can not read file"},
		    #log{date={2019,01,01},time={01,10,00},
			 node=node1,module=module11,line=line11,
			 severity=alarm,
			 info="overload"},
		    #log{date={2019,01,01},time={02,25,00},
			 node=node2,module=module2,line=line2,
			 severity=info,
			 info="info"},
		    #log{date={2019,01,01},time={02,25,30},
			 node=node3,module=module3,line=line3,
			 severity=warning,
			 info="will not do"}, 
		    #log{date={2019,01,01},time={18,43,12},
			 node=node4,module=module4,line=line4,
			 severity=info,
			 info="debug"}
		   ]),
    ok.

t2_match_test()->
    [{log,{2019,1,1},{2,25,0},node2,module2,line2,info,"info"}]=ets:select(log,ets:fun2ms(fun(N=#log{time={H,Min,S},module=Module}) when H>1, Module=:=module2 -> N end)), 

    [{log,{2019,1,1},{2,25,0},node2,module2,line2,info,"info"},
     {log,{2019,1,1},{18,43,12},node4,module4,line4,info,"debug"}]=ets:select(log,ets:fun2ms(fun(N=#log{severity=Severity}) when Severity=:=info-> N end)), 

    ok.


date_str({Y,M,D})->
%2003-10-11T22:14:15.003Z 
    integer_to_list(Y)++"-"++integer_to_list(M)++"-"++integer_to_list(D).
time_str({H,Min,S})->
%2003-10-11T22:14:15.003Z 
    integer_to_list(H)++":"++integer_to_list(Min)++":"++integer_to_list(S).
    
date_time_str({Y,M,D},{H,Min,S})->
    DateStr= integer_to_list(Y)++"-"++integer_to_list(M)++"-"++integer_to_list(D),
    TimeStr=integer_to_list(H)++":"++integer_to_list(Min)++":"++integer_to_list(S),
    DateStr++"T"++TimeStr.
