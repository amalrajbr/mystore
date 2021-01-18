-module(my_image_repository_sup).
-include("my_image_repository.hrl").

-behaviour(supervisor).

-compile(export_all).


start_link()	->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
init([])	->
	ChildSpec = 
		[
		% {alarm_server, {alarm_server, start, []}, permanent, 5000, worker, [alarm_server]}
		% ,{my_image_repository_data, {alarm_data, start, []}, transient, 5000, worker, [alarm_data]}
		{my_image_repository_logger, {my_image_repository_logger, start, []}, permanent, 5000, worker, [my_image_repository_logger]}
		],
	{ok, {{one_for_one, 5, 60}, ChildSpec}}.
	

