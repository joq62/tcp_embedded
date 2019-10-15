%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : Controller to home automation 
%%% Controller shall
%%% 1) load and start services
%%% 2) stop and unload service
%%% 3) Secure that wanted services are running
%%% 4) service discovery support, central and distriubted
%%% 5) Monitor services and nodes
%%% 6) Default connection node for worker nodes that starts and connects
%%% Funkar inte
%%% Start: Each application get requested amount of server_processes(pids)
%%% Stop: All server_processe are released by killing them
%%% Runtime: clients request worker_pids and uses them (allocate) and after usage
%%%          releases them by release (process is killed) 
%%%          Controller ensure that there are right number of processe available 
%%% NodeCrash: Controller checks which Pids are affected starts up services on another node
%%% ProcessCrash: Controller starts an new process on the current node
%%% 
%%% Start: Each application requestes amount of service_processes it needs , done via josca
%%%        Controller creates requested and stores the in ets table as available
%%% Stop:  Controller secure that inuse + avaiable is the amount that was requested before 
%%%        the applications started
%%%
%%% Runtime: clients request worker_pids and uses them (allocate) and after usage
%%%          releases them by release (process is killed) 
%%%          Controller ensure that there are right number of processe available 
%%% ClientCrash: If the client crashes the Controller will kill all related service processes 
%%%              restart the client and secure right amount of service processes 
%%% NodeCrash: Same as ClientCrash but tries to start on another node
%%% service process crash: Controller restart the process 
%%% 
%%% Key data structures
%%% {service_id,[{Node,NumberOfProcesses},,,,,]} 
%%%
%%% {service_id,available,Pid,Ref,Node}
%%% {service_id,inuse,{Pid,Ref},Node,Zone,{usedby_pid,usedby_ref}}
%%% {service_id,service_josca}

%%% {wanted_applications,[application_id,,,,,,]}
%%% {application_status,active|not_started}
%%% {application,application_josca,JoscaTerm}
%%%
%%% {wanted_nodes,[node_id,,,,,]}
%%% {active_nodes,[node_id,,,,,]}

%%% {client_pid,{server_pid,server_ref}
%%% allocated_to_application}
%%% application status
%%% {process,[{service,Pid,Node,zone}]}
%%% Wanted applications
%%% {wanted_applications,[app.josca....]} 
%%% active applications
%%% {active_applicationapp.josca....]} 
%%% Wanted services 
%%% {wanted_service,[service_id,,service_id]}
%%% Active Service: 
%%% {service_id,Pid,Node,zone,NumProcesses}
%%% Active Nodes:
%%% {active_nodes,[Node1,,,NodeN]}
%%% Wanted nodes
%%% {wanted_nodes,[node_id,,,,node_id]}
%%%  
%%% Meta data
%%% service.josca and application.josca
%%%
%%% Algorithm
%%% Workers are just erlang nodes and has no functionality
%%% Controller starts services and monitoring them
%%% Controller monitors workernodes and acts if they goes down
%%% 
%%% Principles:
%%% Each application gets their own set of service instances to manage and will be theirs as long the application
%%% runnning. When the Application is removed the services are removed
%%%
%%% 1) Is it possible to trust monitoring funtion to keep track of all processes ?
%%% 2) If a client allocates one service and then crashes , how will the server be released? 
%%% 3) If a client allocates one service and doesn't give it back 
%%%  
%%% Created : 15 Sept 2019
%%% -------------------------------------------------------------------
-module(brd_ctrl).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


-export([start/1
	 ]).




%% ====================================================================
%% External functions
%% ====================================================================


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
start([])->
    {ok,FileNames}=file:list_dir("."),
    del(FileNames),
    ok.
del([])->
    ok;
del(["Makefile"|T])->
    del(T);
del(["brd_ctrl"|T])->
    del(T);
del([FileName|T]) ->
    os:cmd("rm -rf "++FileName),
    del(T).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
