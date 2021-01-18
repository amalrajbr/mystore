-module(my_image_repository_app).

-behaviour(application).

-export([start/2, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application callback functions

start(_Type, _Args) ->
    case my_image_repository_sup:start_link() of
		{ok, Pid} ->
			my_image_repository_mnesia:start_mnesia(),
			{ok, Pid};
		Error ->
			Error
    end.

stop(_StartArgs) ->
	my_image_repository_mnesia:stop_mnesia(),
  ok.