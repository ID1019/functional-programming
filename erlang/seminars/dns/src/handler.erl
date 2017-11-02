-module(handler).

-compile(export_all).

-define(Port, 53).
-define(Timeout, 5000).


dummy(Packet, IP, Port, _Socket, _Server) ->
    io:format("received request from ~w:~w: ~w~n", [IP, Port, Packet]).



handle(Packet, IP, Port, Socket, Server) ->
    io:format("received request from ~w:~w ~n", [IP, Port]),
    case forward(Packet, Server) of
	{ok, Reply} ->
	    gen_udp:send(Socket, IP, Port, Reply);	    
	{error, Error} ->
	    {error, Error}
    end.
	    

forward(Request, Server) ->
    case gen_udp:open(0, [{active, true}, binary]) of
	{ok, Client} ->
	    gen_udp:send(Client, Server, ?Port, Request),
	    Result = receive 
			 {udp, Client, _IP, _Port, Reply} ->
			     {ok, Reply}
		     after ?Timeout ->
			     {error, timeout}
		     end,
	    gen_udp:close(Client),
	    Result;
	Error ->
	    {error, Error}
    end.






handle(Packet, Rep, Frw) ->
    io:format("handler received request ~n", []),
    case Frw(Packet) of
	{ok, Reply} ->
	    io:format("handler sending reply ~n", []),
	    Rep(Reply);
	{error, Error} ->
	    io:format("error ~w~n", [Error]),
	    {error, Error}
    end.












start(Packet, Repl, Frw) ->
    spawn(fun() -> handler(Packet, Repl, Frw) end).


handler(Packet, Reply,  Frw) ->    
    io:format("handler received request ~w~n", [Packet]),
    trace(Packet),
    case Frw(Packet) of
	{ok, Answer} ->
	    io:format("received reply ~w~n", [Answer]),
	    trace(Answer),
	    Reply(Answer);
	{error, Error} ->
	    io:format("error in forward ~w~n", [Error])
    end.







trace(Packet) ->
    %%io:format("trace ~w~n", [Packet]),
    try msg:decode(Packet) of

	{Id, 0, 0, _, _, RD, _, _, Decoded} ->
	    %% Regular query
	    io:format("~n~nQuery: id= ~w, rd= ~w~n", [Id, RD]),
	    io:format("Body ~p~n~n", [Decoded]);

	{Id, 0, 1, _ , _, RD, _, _, Decoded} ->
	    %% Inverse query
	    io:format("~n~nInverse: id=~w, rd=~w~n", [Id, RD]),
	    io:format("Body ~p~n~n", [Decoded]);

	{Id, 1, _, AA, _, _, RA, 0, Decoded} ->
	    %% Response - ok
	    io:format("~n~nRespons: id= ~w, ra= ~w, aa=~w~n", [Id, RA, AA]),
	    io:format("Body ~p~n~n", [Decoded]);
			  
	{Id, QR, Op, AA, TC, RD, RA, RC, Decoded} ->
	    io:format("~n~nId = ~w~n",[Id]),
	    io:format("This is a query (0) or response (1): ~w~n", [QR]),
	    io:format("The Opcode (0=query,1=iquery,2=status): ~w~n", [Op]),
	    io:format("Authoritative Answer, set to 1 if server is the authority of the domain: ~w~n", [AA]),
	    io:format("Truncated message (if set to 1 there are more messages that follows): ~w~n", [TC]),
	    io:format("Recursive Desired, if the client wants a recursive lookup: ~w~n", [RD]),
	    io:format("Recursive Available, if the server can provide it: ~w~n", [RA]),
	    io:format("Respons Code, ~w~n", [RC]),
	    io:format("Body ~p~n~n", [Decoded])
    catch  
	error:Error ->
	    io:format("decoding failed: ~w~n", [Error])
    end.


    




