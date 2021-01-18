# mystore
This is a repository for my image repository application written using Erlang/OTP22.

Instructions to install the application:
1) Install Erlang in your system
2) Copy all the contents under the 'lib\my_image_repository-1.0.0' directory
  for my case: C:\Program Files\erl10.7\lib\my_image_repository-1.0.0
  
First Time application start:
1) Open start_shell.bat and edit the node name.
for my case: 'node@192.2.2.2'
2) Save and close the file
3) Start the Erlang shell by double clicking on the start_shell.bat file.
4) Enter the following commands
  -> application:start(my_image_repository).
  (Note that fullstop will terminate a statement)
  -> mnesia:change_table_copy_type(schema, node(), disc_copies)
  -> my_image_repository:create_table()
  
Starting second time or later:
1) Start the Erlang shell by double clicking on the start_shell.bat file.
2) Enter the following command
  -> application:start(my_image_repository).
