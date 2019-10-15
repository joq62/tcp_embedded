%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(node_controller).
  


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports


-export([clean_up/1
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
clean_up(Keep)->
    Result=case file:list_dir(".") of
	       {ok,Files}->
		   FilesToDelete=file_to_delete(Files,Keep,Files),
		   delete_files(FilesToDelete),
		   ok;
	       {error,Err} ->
		   {error,Err}
	   end,
    Result.


file_to_delete([],_Keep,FilesToDelete)->
    FilesToDelete;
file_to_delete(_Files,[],FilesToDelete)->
    FilesToDelete;
file_to_delete([File|T],Keep,Acc)->
   % io:format("~p~n",[{?MODULE,?LINE,File,Keep,Acc}]),
    NewAcc= case lists:member(File,Keep)of 
		true->
		    lists:delete(File,Acc);
		false->
		    Acc
	    end,
    file_to_delete(T,Keep,NewAcc).
    

delete_files([])->
    ok;
delete_files([File|T])->
    io:format("~p~n",[{?MODULE,?LINE,"rm -rf "++File}]),
    os:cmd("rm -rf "++File),
    timer:sleep(100),
    delete_files(T).
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
