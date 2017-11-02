-module(relay).

-export([start/0, start/1]).

-define(Server, "8.8.8.8").  %% The Google DNS server

-define(Port, 53).

start() ->
    start(?Server).

start(Server) ->
    spawn_link(fun() -> init(Server) end).


init(Server) ->
    case gen_udp:open(0, [{active, true}, binary]) of
	{ok, Socket} ->
	    relay(1, Socket, Server, []);
	Error ->
	    {error, Error}
    end.


%%% This is a bit of cheating but ok 

relay(N, Socket, Server, Msgs) ->
    receive
	{forward, From, <<Id:16, Request/binary>>} ->
	    io:format("forward message ~w/~w~n", [Id,N]),
	    gen_udp:send(Socket, Server, ?Port, <<N:16, Request/binary>>),
	    relay(N+1, Socket, Server, [{From, N, Id}|Msgs]);
	{udp, Socket, _IP, _Port, <<R:16, Reply/binary>>} ->
	    io:format("reply message ~w~n", [R]),
	    case lists:keyfind(R, 2, Msgs) of
		{From, R, Id} ->
		    From ! {reply, <<Id:16, Reply/binary>>},
		    relay(N, Socket, Server, lists:keydelete(R, 2, Msgs));
		false ->
		    relay(N, Socket, Server, Msgs)
	    end;
	update ->
	    relay:relay(N, Socket, Server, Msgs);
	stop ->
	    ok;
	Strange ->
	    io:format("strange message ~w~n", [Strange]),
	    relay(N, Socket, Server, Msgs)
    end.

    
		        
