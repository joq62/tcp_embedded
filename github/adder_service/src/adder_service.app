%% This is the application resource file (.app file) for the 'base'
%% application.
{application, adder_service,
[{description, "adder_service  " },
{vsn, "1.0.0" },
{modules, 
	  [adder_service_app,adder_service_sup,adder_service,adder]},
{registered,[adder_service]},
{applications, [kernel,stdlib]},
{mod, {adder_service_app,[]}},
{start_phases, []}
]}.
