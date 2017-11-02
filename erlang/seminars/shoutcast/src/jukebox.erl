-module(jukebox).

-export([start_link/1, stop/1, connect/3, next/2]).

-define(Opt,[binary, {packet, 0}, {reuseaddr, true}, {active, true}, {nodelay, true}]).

-define(TimeOut, 5000).

%% The first try, this jukebox will simply stream one song.

start_link(File) ->
    spawn_link(fun() -> init(File) end).

stop(Pid) ->
    Pid ! stop.


init(File) ->
    {mp3, Title, Data} = mp3:read_file(File),
    Header = icy:encode_meta([{title, Title}]),
    Segments = icy:segments(Data),
    receive
	{connect, Client} ->
	    io:format("jukebox: received connection~n",[]),
	    Client ! {header, Header},
	    jukebox(Segments, Data, Client);
	stop ->
	    ok
    end.

connect(Jukebox, _Resource, Client) ->
    Jukebox ! {connect, Client}.

next(Jukebox, Client) ->
    Jukebox ! {next, Client}.    
    


jukebox([], Dacapo, Client) ->
    Segments = icy:segments(Dacapo),    
    jukebox(Segments, Dacapo, Client);
jukebox([{seg, Segment}|Segments], Dacapo, Client) ->
    receive 
	{next, Client} ->
	    Client ! {data, Segment},
	    jukebox(Segments, Dacapo, Client)
    end.
	






    

