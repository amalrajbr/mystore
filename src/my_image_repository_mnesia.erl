-module(my_image_repository_mnesia).

-include("my_image_repository.hrl").

-export([start_mnesia/0
				, stop_mnesia/0
				, create_table/1
				, get_image_data/1
				, insert_image_data/1
				, delete_image_data/1
				]).
				
start_mnesia()	->
	mnesia:start(),
	case  mnesia:table_info(schema, storage_type) of
		disc_copies	-> ok;
		_						->
			mnesia:change_table_copy_type(schema, node(), disc_copies)
	end,
	LocalTables =  mnesia:system_info(local_tables) -- [schema],
	TablesToBeCreated = 
		(fun
			Loop([], Acc)	-> Acc;
			Loop([Table | RestTables], Acc) ->
				case lists:member(Table, LocalTables) of
					true	->	Loop(RestTables, Acc);
					false	->	Loop(RestTables, [Table | Acc])
				end
		end)(?TABLE_LIST, []),
	create_table(TablesToBeCreated).
	
stop_mnesia()		->
	mnesia:stop().

create_table([])	->	ok;
create_table([image_data|RestTables])	->
	mnesia:create_table(image_data, [{disc_copies, [node()]}, {attributes, record_info(fields, image_data)}, {type, ordered_set}]),
	create_table(RestTables).
	
get_image_data(next_id)	->
	case mnesia:dirty_last(image_data) of
		'$end_of_table'	->	1;
		Number -> Number + 1
	end;
get_image_data(select_all)	->
	mnesia:dirty_select(image_data, [{#image_data{_='_'}, [], ['$_']}]);
get_image_data({image_name, Name})		->
	MatchHead = #image_data{image_name = Name, _='_'},
	Result = '$_',
	mnesia:dirty_select(image_data,[{MatchHead, [], [Result]}]);
get_image_data({b64_image_size, Size})		->
	MatchHead = #image_data{b64_image_size = '$1', _='_'},
	Result = '$_',
	mnesia:dirty_select(image_data,[{MatchHead, [{'=<', '$1', Size}], [Result]}]);
get_image_data({raw_image_size, Size})		->
	MatchHead = #image_data{raw_image_size = '$1', _='_'},
	Result = '$_',
	mnesia:dirty_select(image_data,[{MatchHead, [{'=<', '$1', Size}], [Result]}]);
get_image_data({id, Id})		->
	MatchHead = #image_data{id = Id, _='_'},
	Result = '$_',
	mnesia:dirty_select(image_data,[{MatchHead, [], [Result]}]).
	
insert_image_data(ImageData)	->
	mnesia:dirty_write(image_data, ImageData).

delete_image_data(Id)	->	
	mnesia:dirty_delete(image_data, Id).