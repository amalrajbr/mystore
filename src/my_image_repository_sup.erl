-module(my_image_repository_sup).

-include("my_image_repository.hrl").

-behaviour(supervisor).

-compile(export_all).


start_link()	->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
init([])	->
	ChildSpec = 
		[
		{my_image_repository_logger, {my_image_repository_logger, start, []}, permanent, 5000, worker, [my_image_repository_logger]}
		],
	{ok, {{one_for_one, 5, 60}, ChildSpec}}.
	

