%% This is the application resource file (.app file) for the 'base'
%% application.
{application, node_controller_service,
[{description, "node_controller_service  " },
{vsn, "1.0.0" },
{modules, 
	  [node_controller_service_app,node_controller_service_sup,node_controller_service,node_controller]},
{registered,[node_controller_service]},
{applications, [kernel,stdlib]},
{mod, {node_controller_service_app,[]}},
{start_phases, []}
]}.
