%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : Test 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_lib).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% External exports
-export([init/1,init/2,get/1,set/2,get_all/0,delete/1,delete/2,remove/0,remove/1]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/0
%% Description: Type = bag | duplicate_bag | set
%% Returns: {ok, init}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(set)->
    Reply=init(?MODULE,set),
    Reply;
init(bag) ->
    Reply=init(?MODULE,bag),
    Reply;
init(duplicated_bag) ->
    Reply=init(?MODULE,duplicated_bag),
    Reply;
init(Error) ->
    {error,Error}. 

init(FileName,Type) ->
    case filelib:is_file(FileName) of
	false->
	    {ok,Descriptor}=dets:open_file(FileName,[{type,Type}]),
	    dets:close(Descriptor);
	true ->
	    ok_already_existing
    end,
    {ok,init}.

%% --------------------------------------------------------------------
%% Function: set/2
%% Description: Updates the database Key and Value 
%% Returns: {ok,set}|{error,ErrorMsg}
%% --------------------------------------------------------------------
remove()->
    remove(?MODULE).
remove(FileName)->
    file:delete(FileName).

  

%% --------------------------------------------------------------------
%% Function: set/2
%% Description: Updates the database Key and Value 
%% Returns: {ok,set}|{error,ErrorMsg}
%% --------------------------------------------------------------------
set(Key,Value) ->
    Reply=set(Key,Value,?MODULE),
    Reply.

set(Key,Value,FileName) ->
    case dets:open_file(FileName) of
	{ok,Descriptor}->
	    ok=dets:insert(Descriptor, {Key,Value}),
	    Reply={ok,set},
	    dets:close(Descriptor);
	{error,ErrorMsg}->
	    Reply={error,ErrorMsg}
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: set/2
%% Description: Updates the database Key and Value 
%% Returns: {ok,set}|{error,ErrorMsg}
%% --------------------------------------------------------------------
delete(Key) ->
    Reply=delete(Key,?MODULE),
    Reply.
delete(Key,FileName)->
    case dets:open_file(FileName) of
	{ok,Descriptor}->
	    Reply=dets:delete(Descriptor,Key),
	    dets:close(Descriptor);
	{error,ErrorMsg}->
	    Reply={error,ErrorMsg}
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: get/1, get/2
%% Description: Retreive Value based on Key 
%% Returns: {ok,Value}|{error,ErrorMsg}
%% --------------------------------------------------------------------
get(Key) ->
    Reply=get(Key,?MODULE),
    Reply.

get(Key,FileName) ->
    case dets:open_file(FileName) of
	{ok,Descriptor}->
	    case dets:lookup(Descriptor, Key) of
		[]->
		    Reply = {error,no_entry};
		X->
		    [{Key,Value}]=X,
		    Reply={ok,Value}
	    end,
	    dets:close(Descriptor);
	{error,ErrorMsg}->
	    Reply={error,ErrorMsg}
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: get/1, get/2
%% Description: Retreive Value based on Key 
%% Returns: {ok,Value}|{error,ErrorMsg}
%% --------------------------------------------------------------------
   

%% --------------------------------------------------------------------
%% Func: init
%% Purpose: Init the dets file with intial set up 
%% Returns: {ok,ok}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Func: get_all/
%% Purpose: creates a list of all items in the dets  
%% Returns: {ok,ListOfItems}|{error,ErrMsg}
%% --------------------------------------------------------------------

get_all()->
    Reply=get_all(?MODULE),
    Reply.
get_all(FileName)->
    case dets:open_file(FileName) of
	{ok,Desc}->
	    Key=dets:first(Desc),
	    Reply=get_all(Desc,Key,[]);
	{error,ErrMsg}->
	    Reply={error,ErrMsg}
    end,
    Reply.    

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Func: get_all/3
%% Purpose: Support function to get_all , retrieves all Values  
%% Returns: {ok,ListOfItems}
%% --------------------------------------------------------------------
get_all(Desc,'$end_of_table',Acc)->
    dets:close(Desc),
    {ok,Acc};
get_all(Desc,Key,Acc)->  
    [Status]=dets:lookup(Desc, Key),
    Acc1=[Status|Acc],
    Key1=dets:next(Desc,Key),
    get_all(Desc,Key1,Acc1).
