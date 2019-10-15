%% This is the application resource file (.app file) for the 'base'
%% application.
{application, lib_service,
[{description, "lib_service  " },
{vsn, "1.0.0" },
{modules, 
	  [lib_service_app,lib_service_sup,lib_service,tcp_client,tcp_server]},
{registered,[lib_service]},
{applications, [kernel,stdlib]},
{mod, {lib_service_app,[]}},
{start_phases, []}
]}.
