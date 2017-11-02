transport(Net, Open) ->
    receive 
	{listen, Port, App} ->
	    Socket = listen:start(App, Port, Net),
	    erlang:monitor(process, Socket),
	    transport(Net, [{Port, Socket} | Open]);

	{connect, Port, DestId, DestPort} ->
	    Socket = socket:start(App, Port, DestPort, DestId),
	    transport(Net, [{Port, Socket} | Open]);
	    
	{'DOWN', _, process, Socket, _} ->
	    transport(Net, lists:keydelete(Socket, 2, Open));

	{msg, To, Msg} =  ->
	    case lists:keyfind(To, 1, Open) of
		{To, Socket}  ->
		    Socket ! {msg, Msg},
		    transport(Net, Open);		    
		false ->
		    transport(Net, Open)
	    end		    
    end.
