-module(listen).

-compile(export_all).


start(App, Port, Net) ->
    spawn(fun() -> init(App, Port, Net) end).

init(App, Port, Net) ->
    App ! {ok, self()},
    listen(Port, Net).


listen(Port, Net) ->
    receive 
	{listen, App} ->
	    %% add App to listening applications
	    listen(Port, Net);	    

        {connect, To, From} ->
	    %% spawn a socket process and send to listening app
	    listen(Port, Net);
	
	{msg, _, From} ->
	    %% send it to the right socket
	    listen(Port, Net);
	 ->
    

    
		   


    
