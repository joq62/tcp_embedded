%% This is the application resource file (.app file) for the 'base'
%% application.
{application, controller_service,
[{description, "controller_service" },
{vsn, "0.0.1" },
{modules, [controller_service_app,controller_service_sup,
	   controller_service,controller,
	   node_config]},
{registered,[controller_service]},
{applications, [kernel,stdlib]},
{mod, {controller_service_app,[]}},
{start_phases, []}
]}.
