%% This is the application resource file (.app file) for the 'base'
%% application.
{application, ets_service,
[{description, "ets_service  " },
{vsn, "1.0.0" },
{modules, 
	  [ets_service_app,ets_service_sup,ets_service,ets_lib]},
{registered,[ets_service]},
{applications, [kernel,stdlib]},
{mod, {ets_service_app,[]}},
{start_phases, []}
]}.
