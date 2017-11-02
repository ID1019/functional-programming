-module(proxy1).

-export([start/1, start_link/1, stop/1]).


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

start(Jukebox) ->
    spawn(fun() -> init(Jukebox) end).

start_link(Jukebox) ->
    spawn_link(fun() -> init(Jukebox) end).

stop(Pid) ->
    Pid ! stop.

init(Jukebox) ->
    {ok, Listen} = gen_tcp:listen(?Port, ?Opt),
    proxy(Jukebox, Listen).

proxy(Jukebox, Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("proxy: connect~n",[]),
    case read_request(Socket) of
	{ok, {get, Request, _Headers, _Version}, _Rest} ->
	    io:format("proxy: received request ~p~n", [Request]),
	    jukebox:connect(Jukebox, Request, self()),
	    receive
		{header, Header} ->
		    io:format("proxy: received header ~w~n", [Header]),
		    gen_tcp:send(Socket, icy:encode_response(?Header)),
		    loop(Header, Jukebox, Socket);
		{error, Error} ->
		    io:format("proxy: ~s~n", [Error])
	    end;
	{error, Error} ->
	    io:format("proxy: ~s~n", [Error])
    end.



loop(Header, Jukebox, Socket) ->
    jukebox:next(Jukebox, self()),
    receive
	{header, New} ->
	    loop(New, Jukebox, Socket);
	{data, Segment} ->
	    io:format("proxy: received data~n", []),
	    gen_tcp:send(Socket, Segment),
	    io:format("proxy: sent segment ~n", []),
	    gen_tcp:send(Socket, Header),	    
	    io:format("proxy: sent header ~n", []),
	    loop(Header, Jukebox, Socket);
	{tcp_closed, Socket} ->
	    io:format("proxy: connection closed~n",[]),
	    ok;
	stop ->
	    ok
    after ?TimeOut ->
	    io:format("proxy: time-out~n", []),
	    error
    end.





%% The icy module interface.

read_request(Socket) ->
    reader(fun()-> icy:decode_request(<<>>) end, Socket).
	    
reader(Cont, Socket) ->
    case Cont() of
	{ok, Parsed, Rest} ->
	    {ok, Parsed, Rest};
	{more, Fun} ->
	    receive
		{tcp, Socket, More} ->
		    reader(fun() -> Fun(More) end,  Socket);
		{tcp_closed, Socket} ->
		    {error, "server closed connection"}
	    after ?TimeOut ->
		    {error, "time out"}
	    end;
	{error, Error} ->
	    {error, Error}
    end.


    

