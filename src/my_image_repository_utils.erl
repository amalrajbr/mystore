-module(my_image_repository_utils).
-export([
				insert_image/1
				, delete_image/1
				, filter_image/1
				]).

-include("my_image_repository.hrl").

insert_image([]) ->
	{ok, success};
insert_image([Head | Rest]) ->
	case insert_image(Head) of
		ok							->	insert_image(Rest);
		{error, Reason}	->	{error, Reason}
	end;
insert_image(ImageData) when is_record(ImageData, image_data) ->
	case catch insert_image1(ImageData) of
		{error, Reason}	->
			{error, Reason};
		{ok, Data}			->
			{ok, Data}
	end;
insert_image(_ImageData) ->
	{error, invalid_data}.
	
insert_image1(ImageData) when is_record(ImageData, image_data) ->
	ImageLocation = ImageData#image_data.b64_image,
	case file:open(ImageLocation) of
		{error, Reason}		->	{error, Reason};
		{ok, BinaryData}	->
			B64EncodedImage =
				case base64:encode(BinaryData) of
					{error, Reason1}	->	throw({error, Reason1});
					B64ImageData		->	B64ImageData
				end,
			Id = my_image_repository_mnesia:get_image_data(next_id),
			B64ImageSize = size(B64EncodedImage),
			RawImageSize = size(BinaryData),
			NewImageData = ImageData#image_data{id = Id,
													b64_image = B64EncodedImage,
													b64_image_size = B64ImageSize,
													raw_image_size = RawImageSize},
			case my_image_repository_mnesia:insert_image_data(NewImageData) of
				ok								->	ok;
				{error, Reason2}	->	{error, Reason2}
			end
	end.

delete_image([])	->
	{ok, success};
delete_image([Head | Rest])	->
	case delete_image(Head) of
		ok							->	insert_image(Rest);
		{error, Reason}	->	{error, Reason}
	end;
delete_image(Id) when is_integer(Id)	->
	mnesia:dirty_delete(image_data, Id);
delete_image(_Id)											->
	{error, invalid_data}.

filter_image({id, Id})								->
	case my_image_repository_mnesia:get_image_data({id, Id}) of
		[]					->	{ok, no_data};
		[ImageData]	->	{ok, ImageData}
	end;
filter_image({image_name, ImageName})	->
	case my_image_repository_mnesia:get_image_data({image_name, ImageName}) of
		[ImageData]	->	{ok, ImageData};
		[]					->
			case my_image_repository_mnesia:get_image_data(select_all) of
				[]			->	{ok, no_data};
				Images	->
					FilteredImages = 
						(fun
							Loop([], Acc)				->	Acc;
							Loop([H|Rest], Acc)	->
								case binary:match(H#image_data.image_name, ImageName) of
									nomatch	->	Loop(Rest, Acc);
									_Match	->	Loop(Rest, [H|Acc])
								end
						end)(Images, []),
					case FilteredImages of
						[]	->	{ok, no_data};
						_		->	{ok, FilteredImages}
					end
			end
	end;
filter_image({image_size, Size})			->
	case my_image_repository_mnesia:get_image_data({image_size, Size}) of
		[]					->	{ok, no_data};
		[ImageData]	->	{ok, ImageData}
	end.