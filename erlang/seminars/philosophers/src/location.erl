-module(location).

-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init() ->
    available().

available() ->
    receive 
	{request, From} ->
	    From ! {granted, self()},
	    empty()
    end.

empty() ->
    receive 
	returned ->
	    available()
    end.


		   
