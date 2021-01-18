-module(my_image_repository_logger).
-include("my_image_repository.hrl").

-export([
				start/0
				, log/2
				, stop_log/0
				
				, init/1
				, handle_call/3
				, handle_cast/2
				, terminate/2]).

-behaviour(gen_server).

start()	->
	gen_server:start_link({local,?MODULE}, ?MODULE, [],[]).


check(FileName, No)	->
	case filelib:is_file(FileName) of
		false	->
			{ok, FileHandler} = file:open(FileName, [write]),
			{ok, {FileName, FileHandler}};
		true	->
			{A,B,C} = date(),
			FileName_New =  ?LOG_DIR_N(io_lib:format("~p-~p-~p_~p",[A,B,C, No])),
			check(FileName_New, No + 1)
	end.
			
	
log({Module, Pid},{error, Data})	->
	gen_server:cast(?MODULE, {errorlog, Module, Pid, Data});
log({Module, Pid},{success, Data})	->
	gen_server:cast(?MODULE, {successlog, Module, Pid, Data}).

stop_log()	->
	gen_server:cast(?MODULE, stop).


	
init([])	->
	{A,B,C} = date(),
	FileName =  ?LOG_DIR(io_lib:format("~p-~p-~p",[A,B,C])),
	check(FileName,1).
	
	
handle_cast({errorlog, Module, Pid, Data}, {FileName, FileHandler})	->
	file:write(FileHandler, io_lib:format("ERROR on ~p in module ~p where pid is ~p, data is ~p\r\n",[calendar:local_time(), Module, Pid, Data])),
	{noreply, {FileName, FileHandler}};
handle_cast({successlog, Module, Pid, Data}, {FileName, FileHandler})	->	
	file:write(FileHandler, io_lib:format("SUCCESS on ~p in module ~p where pid is ~p, data is ~p\r\n",[calendar:local_time(), Module, Pid, Data])),
	{noreply, {FileName, FileHandler}};
handle_cast(stop, {FileName, FileHandler})	->
	file:write(FileHandler, lists:flatten(io_lib:format("LOGGING STOPPED:\tFile name: ~p, File handler: ~p\r\n",[FileName,FileHandler]))),
	{stop, normal, {FileName,FileHandler}}.
	
handle_call(_, _From, _State)	->
	{reply, [], _State}.
	
terminate(_,_)	->
	ok.
