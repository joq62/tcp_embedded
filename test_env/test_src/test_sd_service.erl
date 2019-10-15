%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : eunit for service discovery  
%%% Nodes -> used for manage Pods (erlang vm) 
%%% Pods -> used for manage containers (erlang vm)
%%% Container -> manage services (erlang applications)
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_sd_service). 
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------

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
start()->
    {ok,Host}=inet:gethostname(),

    WorkerNodeId="node_worker_1",
    WorkerNodeStr=WorkerNodeId++"@"++Host,
    WorkerNode=list_to_atom(WorkerNodeStr),
    MasterNodeId="node_master_1",
    MasterNodeStr=MasterNodeId++"@"++Host,
    MasterNode=list_to_atom(MasterNodeStr),

    WorkerPodId="pod_worker_1",
    WorkerPodStr=WorkerPodId++"@"++Host,
    WorkerPod=list_to_atom(WorkerPodStr),

    WorkerPodId_2="pod_worker_2",
    WorkerPodStr_2=WorkerPodId_2++"@"++Host,
    WorkerPod_2=list_to_atom(WorkerPodStr_2),

    MasterPodId="pod_master_1",
    MasterPodStr=MasterPodId++"@"++Host,
    MasterPod=list_to_atom(MasterPodStr),
    ControllerPodId="pod_controller",
    ControllerPodStr=ControllerPodId++"@"++Host,
    ControllerPod=list_to_atom(ControllerPodStr),
  
    ok=rpc:call(ControllerPod,sd_service,add_worker_node,[WorkerNodeId,WorkerNode]),
    ok=rpc:call(ControllerPod,sd_service,add_master_node,[MasterNodeId,MasterNode]),

    ok=rpc:call(ControllerPod,sd_service,add_worker_pod,[WorkerPodId,WorkerPod]),
    ok=rpc:call(ControllerPod,sd_service,add_master_pod,[MasterPodId,MasterPod]),

    ok=rpc:call(ControllerPod,sd_service,register_service,["ServiceId_1",WorkerPod]),
    ok=rpc:call(ControllerPod,sd_service,register_service,["ServiceId_1",WorkerPod_2]),
    ok=rpc:call(ControllerPod,sd_service,register_service,["Master_1",MasterPod]),

%get_info_test()->    

    [{"node_worker_1",node_worker_1@asus}]=rpc:call(ControllerPod,sd_service,worker_nodes,[]), % node_worker_id@Host All workers in the system(clustere)
    [{"node_master_1",node_master_1@asus}]=rpc:call(ControllerPod,sd_service,master_nodes,[]), % node_master_id@Host All master in the system(clustere)
    [{"pod_worker_1",pod_worker_1@asus}]=rpc:call(ControllerPod,sd_service,worker_pods,[]), % pod_worker_id@Host wrokerAll pods on node Node
    [{"pod_master_1",pod_master_1@asus}]=rpc:call(ControllerPod,sd_service,master_pods,[]),

    [{{"node_worker_1",node_worker_1@asus},_T100}]=rpc:call(ControllerPod,sd_service,worker_nodes_time,[]), % node_worker_id@Host All workers in the system(clustere)
    [{{"node_master_1",node_master_1@asus},_T101}]=rpc:call(ControllerPod,sd_service,master_nodes_time,[]), % node_master_id@Host All master in the system(clustere)
    [{{"pod_worker_1",pod_worker_1@asus},_T102}]=rpc:call(ControllerPod,sd_service,worker_pods_time,[]), % pod_worker_id@Host wrokerAll pods on node Node
    [{{"pod_master_1",pod_master_1@asus},_T103}]=rpc:call(ControllerPod,sd_service,master_pods_time,[]),

    [{"Master_1",pod_master_1@asus},
     {"ServiceId_1",pod_worker_2@asus},
     {"ServiceId_1",pod_worker_1@asus}]=rpc:call(ControllerPod,sd_service,services,[]), 

    [{{"Master_1",pod_master_1@asus},_T1},
           {{"ServiceId_1",pod_worker_2@asus},_T2},
           {{"ServiceId_1",pod_worker_1@asus},_T3}]=rpc:call(ControllerPod,sd_service,services_time,[]), 

    [{"ServiceId_1",pod_worker_2@asus},
     {"ServiceId_1",pod_worker_1@asus}]=rpc:call(ControllerPod,sd_service,service,["ServiceId_1"]),
    [{"Master_1",pod_master_1@asus}]=rpc:call(ControllerPod,sd_service,service,["Master_1"]),
    []=rpc:call(ControllerPod,sd_service,service,["glurk"]),

    [{{"ServiceId_1",pod_worker_2@asus},_T10},
     {{"ServiceId_1",pod_worker_1@asus},_T11}]=rpc:call(ControllerPod,sd_service,service_time,["ServiceId_1"]),
    [{{"Master_1",pod_master_1@asus},_T12}]=rpc:call(ControllerPod,sd_service,service_time,["Master_1"]),
    []=rpc:call(ControllerPod,sd_service,service_time,["glurk"]),


%delete_info_test()->
    ok=rpc:call(ControllerPod,sd_service,delete_worker_node,[WorkerNodeId,WorkerNode]),
    ok=rpc:call(ControllerPod,sd_service,delete_master_node,[MasterNodeId,MasterNode]),
    []=rpc:call(ControllerPod,sd_service,worker_nodes,[]),
    []=rpc:call(ControllerPod,sd_service,master_nodes,[]),

    ok=rpc:call(ControllerPod,sd_service,delete_worker_pod,[WorkerPodId,WorkerPod]),
    ok=rpc:call(ControllerPod,sd_service,delete_master_pod,[MasterPodId,MasterPod]),
    []=rpc:call(ControllerPod,sd_service,worker_pods,[]),
    []=rpc:call(ControllerPod,sd_service,master_pods,[]),

    ok=rpc:call(ControllerPod,sd_service,delete_service,["ServiceId_1",WorkerPod]),
    ok=rpc:call(ControllerPod,sd_service,delete_service,["ServiceId_1",WorkerPod_2]),
    ok=rpc:call(ControllerPod,sd_service,delete_service,["Master_1",MasterPod]),
    []=rpc:call(ControllerPod,sd_service,services,[]), 
    



%delete_info_2_test()->


    ok=rpc:call(ControllerPod,sd_service,delete_worker_node,[WorkerNodeId,WorkerNode]),
    ok=rpc:call(ControllerPod,sd_service,delete_master_node,[MasterNodeId,MasterNode]),
    []=rpc:call(ControllerPod,sd_service,worker_nodes,[]),
    []=rpc:call(ControllerPod,sd_service,master_nodes,[]),

    ok=rpc:call(ControllerPod,sd_service,delete_worker_pod,[WorkerPodId,WorkerPod]),
    ok=rpc:call(ControllerPod,sd_service,delete_master_pod,[MasterPodId,MasterPod]),
    []=rpc:call(ControllerPod,sd_service,worker_pods,[]),
    []=rpc:call(ControllerPod,sd_service,master_pods,[]),

    ok=rpc:call(ControllerPod,sd_service,delete_service,["ServiceId_1",WorkerPod]),
    ok=rpc:call(ControllerPod,sd_service,delete_service,["ServiceId_1",WorkerPod_2]),
    ok=rpc:call(ControllerPod,sd_service,delete_service,["Master_1",MasterPod]),
    []=rpc:call(ControllerPod,sd_service,services,[]), 
    
    ok.
