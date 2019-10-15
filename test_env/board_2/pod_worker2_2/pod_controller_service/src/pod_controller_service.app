%% This is the application resource file (.app file) for the 'base'
%% application.
{application, pod_controller_service,
[{description, "pod_controller_service  " },
{vsn, "1.0.0" },
{modules, 
	  [pod_controller_service_app,pod_controller_service_sup,pod_controller_service,pod_controller]},
{registered,[pod_controller_service]},
{applications, [kernel,stdlib]},
{mod, {pod_controller_service_app,[]}},
{start_phases, []}
]}.
