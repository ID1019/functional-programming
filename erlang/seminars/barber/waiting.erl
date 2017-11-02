-module(waiting).

-export([start/1]).


start(Max) ->
    {ok, spawn_link(fun() -> init(Max) end)}.

init(Max) ->
    waiting(Max, []).

waiting(0, []) ->
    io:format("should not happen ~n", []),
    ok;
waiting(0, [Next|Rest]=Queue) ->
    receive 
	close ->
	    ok;
	{enter, Customer} ->
	    Customer ! sorry,
	    waiting(0, Queue);
	{next, Barber} ->
	    Barber ! {ok, Next},
	    waiting(1, Rest)
    end;
waiting(N, []) ->
    receive
	close ->
	    ok;
	{enter, Customer} ->
	    Customer ! please_wait,
	    waiting(N-1, [Customer])
    end;
waiting(N, [Next|Rest]=Queue) ->
    receive
	close ->
	    ok;
	{next, Barber} ->
	    Barber ! {ok, Next},
	    waiting(N+1, Rest);
	{enter, Customer} ->
	    Customer ! please_wait,
	    waiting(N-1, Queue ++ [Customer])
    end.

