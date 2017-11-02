-module(dns).

%%% https://www.ietf.org/rfc/rfc1035.txt

-compile(export_all).

-define(Server, "8.8.8.8").  %% The Google DNS server

-define(Port, 53).

-define(Local, 5300).

-define(Timeout, 4000).

start() ->
    start(?Local, ?Server).

start(Port) ->
    start(Port, ?Server).    

start(Port, Server) -> 
    spawn_link(fun() -> init(Port, Server) end).

init(Port, Server) ->
    case gen_udp:open(Port, [{active, true}, binary]) of
	{ok, Socket} ->
	    dns(Socket, Server);
	Error ->
	    io:format("dns error opening server socket ~w~n", [Error])
    end.
    


dns(Socket, Server) ->
    receive 
	{udp, Socket, IP, Port, Packet} ->
	    Reply = fun(Rep) -> gen_udp:send(Socket, IP, Port, Rep)  end,
	    Frw = fun(Req) -> forward(Req, Server) end,
 	    handler:start(Packet, Reply, Frw),

	    dns(Socket, Server);
	update ->
	    dns:dns(Socket, Server);
	stop ->
	    io:format("by by~n", []),
	    ok;
	Error ->
	    io:format("strange message ~w~n", [Error]),
	    dns(Socket, Server)
    end.






%         Reply = fun(Rep) -> reply(Rep, Socket, IP, Port) 
%          Frw = fun(Req) -> forward(Req, Server) end,


% 	    handler:handle(Packet, Reply, Frw),

%	    handler:start(Packet, Reply, Frw),

%   	    handler:handle(Packet, IP, Port, Socket, Server),





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
    


