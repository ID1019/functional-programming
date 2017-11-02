-module(server).

%%% https://www.ietf.org/rfc/rfc1035.txt

-compile(export_all).

-define(Server, "8.8.8.8").  %% The Google DNS server

-define(RemotePort, 53).

-define(LocalPort, 5300).

-define(Timeout, 4000).

start() ->
    start(?LocalPort, ?Server).

start(LocalPort) ->
    start(LocalPort, ?Server).    

start(LocalPort, Server) -> 
    spawn_link(fun() -> init(LocalPort, Server) end).

init(LocalPort, Server) ->

    case gen_udp:open(LocalPort, [{active, true}, binary]) of
	{ok, Local} ->
	        case gen_udp:open(0, [{active, true}, binary]) of
		    {ok, Remote} ->
			dns(Local, Remote, Server, 1, []);
		     Error ->
			io:format("dns error opening server socket ~w~n", [Error])
		end;
	Error ->
	    io:format("dns error opening server socket ~w~n", [Error])
    end.

dns(Local, Remote, Server, N, Msgs) ->
    receive 
	{udp, Local, IP, Port, <<Id:16, Request/binary>>} ->
	    io:format("request ~w ~w ~w~n", [IP, Port, Id]),
	    gen_udp:send(Remote, Server, ?RemotePort, <<N:16, Request/binary>>),
	    dns(Local, Remote, Server, N+1, [{N, IP, Port, Id}|Msgs]);	    

	{udp, Remote, _IP, _Port, <<R:16, Reply/binary>>} ->
	    io:format("reply message ~w~n", [R]),
	    case lists:keyfind(R, 1, Msgs) of
		{R, IP, Port, Id} ->
		    gen_udp:send(Local, IP, Port, <<Id:16, Reply/binary>>),
		    dns(Local, Remote, Server, N, lists:keydelete(R, 1, Msgs));
		false ->
		    dns(Local, Remote, Server, N, Msgs)
	    end;

	update ->
	    server:dns(Local, Remote, Server, N, Msgs);

	stop ->
	    io:format("by by~n", []),
	    ok;

	Error ->
	    io:format("strange message ~w~n", [Error]),
	    dns:dns(Local, Remote, Server, N, Msgs)
    end.




    




