
%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to module_org
-module(test_ssl).

%%
%% Include files
%%
-define(PORT,10100).
-define(CERTFILE,"certificate.pem").
-define(KEYFILE,"key.pem").
-define(TYPE,parallell).
-define(ADDR,"80.216.90.144").
%-define(ADDR,"localhost").
%%
%% Exported Functions
%%
-export([start/0,server/0, client/0,
	cmd/1]).

%%
%% API Functions
%%
cmd(Arg)->
    case Arg of
	{test,1}->
	   % ReplyTerm={date(),time()};
	ReplyTerm=1;
	{test,2}-> 
	 %   {ok,Bin}=file:read_file("/home/joq/Pictures/2006_04_09__09_59_32.JPG"),
	 %   ReplyTerm=Bin;
	    ReplyTerm=2;
	{test,3}->
	    ReplyTerm=3;
	_ ->
	    ReplyTerm="no action"
    end,
   % io:format(" ~p~n",[{?MODULE,?LINE,Arg}]),
    ReplyTerm.


start()->
    spawn(fun()->server() end),
    timer:sleep(2000),
    spawn(fun()->client() end).

server()->
    ServerPid=self(),
    io:format("ServerPid = ~p~n",[{?MODULE,?LINE,ServerPid}]),
    SslPid=spawn(fun()->ssl_lib:start({test_ssl,cmd},?PORT,?CERTFILE,?KEYFILE,?TYPE) end), 
 %  loop(SslPid).
    ok.
loop(SslPid)->
  %  io:format("SslPid= ~p~n",[{?MODULE,?LINE,SslPid}]),
    ServerPid=self(),
   % io:format("ServerPid = ~p~n",[{?MODULE,?LINE,ServerPid}]),
    receive
	{_SslPid,{ssl_req,MsgTerm}}->
	   % io:format("MsgTerm = ~p~n",[{?MODULE,?LINE,MsgTerm}]),
	    ReplyTerm=date(),
	    SslPid!{self(),{ssl_response,ReplyTerm}};
	X ->
	    io:format("unmatched signal ~p~n",[{?MODULE,?LINE,X}]),
	    io:format("SslPid= ~p~n",[{?MODULE,?LINE,SslPid}])
    end,
  loop(SslPid).  
    
client()->
    ssl:start(),
    {ok,S}=ssl_lib:connect(?ADDR,?PORT),
    client_loop(S,0,0),
    ssl_lib:disconnect(S).

client_loop(S,N,TotalTime)->
    {T,1}=timer:tc(ssl_lib,call,[S,{test,1}]),
    N1=N+1,
    TotalTime1=TotalTime+T,
    Signals_second=1/T*1000000,
    case N1 rem 100 of
	0 ->
	    io:format("Mean time = ~p~n",[{TotalTime1/N1/1000}]),
	    io:format("signals per second = ~p~n",[{1/T*1000000}]);
	X ->
	    do_nothing
    end,
    
	    	
  %  io:format("test1 = ~p~n",[{?MODULE,?LINE,ssl_lib:call(?ADDR,?PORT,{test,1})}]),
  %  timer:sleep(1000),
   % io:format("test2 = ~p~n",[{?MODULE,?LINE,ssl_lib:call(?ADDR,?PORT,{test,2})}]),
   % timer:sleep(1000),
   % io:format("test3 = ~p~n",[{?MODULE,?LINE,ssl_lib:call(?ADDR,?PORT,{test,3})}]),
 
%   timer:sleep(2),
    
    client_loop(S,N1,TotalTime1).

