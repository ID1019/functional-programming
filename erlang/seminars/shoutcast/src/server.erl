-module(server).

-export([start/1]).

-define(Opt,[binary, {packet, 0}, {reuseaddr, true}, {active, true}, {nodelay, true}]).

-define(Port, 8080).

-define(TimeOut, 5000).

-define(Header, [{'icy-name', "BEST"}, {'icy-genre', "Groove"}, {'icy-notice1', "Our own Jukebox"}]).


%% The proxy will listen to a port and connect one music player over
%% ICY/HTTP. The proxy will then access the jukebox and start
%% streaming. The proxy will send segments as fast as is allowed by
%% the TCP connection, the client process will thus do the flow
%% control. The jukebox will send segments to the proxy on demand in
%% order not to overflow the proxy. 

start(File) ->
    spawn(fun() -> init(File) end).

init(File) ->
    {mp3, Title, Data} = mp3:read_file(File),
    Header = icy:encode_meta([{title, Title}]),
    Segments = icy:segments(Data),
    {ok, Listen} = gen_tcp:listen(?Port, ?Opt),
    server(Header, Segments, Listen).

server(Header, Segments, Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("server: connect~n",[]),
    case read_request(Socket) of
	{ok, {get, Request, _Headers, _Version}, _Rest} ->
	    io:format("server: received request ~p~n", [Request]),
	    gen_tcp:send(Socket, icy:encode_response(?Header)),
	    loop(Header, Segments, Socket, 0);
	{error, Error} ->
	    io:format("server: ~s~n", [Error])
    end.


loop(_, [], _, _) ->
     io:format("server: done ~n", []),
    ok;
loop(Header, [{seg, Segment}|Rest], Socket, N) ->
    gen_tcp:send(Socket, Segment),
    io:format("server: sent segment ~w ~n", [N]),
    gen_tcp:send(Socket, Header),	    
    io:format("server: sent header ~n", []),
    loop(Header, Rest, Socket, N+1).



%% The icy module interface.

read_request(Socket) ->
    read_request(fun(More) -> icy:decode_request(More) end, Socket).

read_request(Cont, Socket) ->
    receive
	{tcp, Socket, Segment} ->	
	    case Cont(Segment) of
		{ok, Request, Body} ->
		    {ok, Request, Body};
		{more, More} ->
		    read_request(More, Socket)
	    end;
	{tcp_closed, Socket}  ->
	    {error, "socket closed"}
    end.






    

