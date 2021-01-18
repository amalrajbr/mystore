% LOGGING PRIVATE DEFINE
-define(LOG_DIR(X), code:lib_dir(my_image_repository) ++ "/logs/log-" ++ X ++ "_1.txt").
-define(LOG_DIR_N(X), code:lib_dir(my_image_repository) ++ "/logs/log-" ++ X ++ ".txt").

% DEBUGGING PRIVATE DEFINE
-define(PRINT(X), io:format("\r\n~p\r\n",[X]).

%%	MNESIA RECORD INFO
% IMAGE DATA RECORD
-record(image_data, 
					{id
					, image_name
					, b64_image
					, raw_image_size
					, b64_image_size
					}).
					
-define(TABLE_LIST, [image_data]).