-module(my_image_repository_utils).
-export([
				insert_images/1
				, delete_images/1
				, filter_image/1
				]).

-include("my_image_repository.hrl").

insert_images(Image) when is_record(Image, image_data) ->
	insert_image([Image], []);
insert_images(Images) ->
	insert_image(Images, []).

insert_image([], Responses) ->
	{ok, Responses};
insert_image([Head | Rest], Responses) ->
	case catch insert_image(Head) of
		ok							->
			insert_image(Rest
									, [{Head#image_data.image_name, success} | Responses]);
		{X, Reason}	when X == 'EXIT'; X == error ->
			insert_image(Rest
									, [{Head#image_data.image_name, failure, Reason} | Responses])
	end.

insert_image(ImageData) when is_record(ImageData, image_data) ->
	case catch insert_image1(ImageData) of
		{X, Reason}	when X == 'EXIT'; X == error ->
			{error, Reason};
		ok	->	ok
	end;
insert_image(_ImageData) ->
	{error, invalid_data}.
	
insert_image1(ImageData)	->
	ImageName = validate_name(ImageData#image_data.image_name),
	ImageLocation = ImageData#image_data.b64_image,
	case file:read_file(ImageLocation) of
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
													image_name = ImageName,
													b64_image = B64EncodedImage,
													b64_image_size = B64ImageSize,
													raw_image_size = RawImageSize},
			case my_image_repository_mnesia:insert_image_data(NewImageData) of
				ok								->	ok;
				{error, Reason2}	->	{error, Reason2}
			end
	end.

delete_images(Id) when is_integer(Id) ->
	delete_image([Id], []);
delete_images(Ids)	->
	delete_image(Ids, []).

delete_image([], Responses)	->
	{ok, Responses};
delete_image([Id | Rest], Responses)	->
	case catch delete_image(Id) of
		ok							->
			delete_image(Rest, [{Id, success} | Responses]);
		{X, Reason}	when X == 'EXIT'; X == error ->
			delete_image(Rest, [{Id, failure, Reason} | Responses])
	end.

delete_image(Id) when is_integer(Id)	->
	mnesia:dirty_delete(image_data, Id);
delete_image(_Id)											->
	{error, invalid_data}.

filter_image(select_all)								->
	case my_image_repository_mnesia:get_image_data(select_all) of
		[]					->	{ok, no_data};
		ImageData	->	{ok, ImageData}
	end;
filter_image({id, Id})								->
	case my_image_repository_mnesia:get_image_data({id, Id}) of
		[]					->	{ok, no_data};
		[ImageData]	->	{ok, ImageData}
	end;
filter_image({image_name, ImageName})	->
	case my_image_repository_mnesia:get_image_data({image_name, ensure_binary(ImageName)}) of
		[]					->	{ok, no_data};
		ImageData		->	{ok, ImageData}
	end;
filter_image({ImageSize, Size}) when ImageSize == raw_image_size
																; ImageSize == b64_image_size	->
	case my_image_repository_mnesia:get_image_data({ImageSize, Size}) of
		[]					->	{ok, no_data};
		ImageData		->	{ok, ImageData}
	end.
	
validate_name(ImageName)	->
	BinaryImageName = ensure_binary(ImageName),
	case binary_part(BinaryImageName, {byte_size(BinaryImageName), -3}) of
		<<"jpg">>	->	BinaryImageName;
		<<"png">>	->	BinaryImageName;
		_					->	throw({error, invalid_name})
	end.

ensure_binary(X) when is_list(X) -> list_to_binary(X);
ensure_binary(X)  when is_binary(X)	-> X;
ensure_binary(_X)	->	<<>>.