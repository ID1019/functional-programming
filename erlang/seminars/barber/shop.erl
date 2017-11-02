-module(shop).

-export([start/1]).

start(Max) ->
    {ok, spawn_link(fun() -> init(Max) end)}.

init(Max) ->
    {ok, Waiting} = waiting:start(Max),
    {ok, Barber} = barber:start(Waiting),
    shop(Waiting, Barber).

shop(Waiting, Barber) ->
    receive 
	{hello, Customer} ->
	    Waiting ! {enter, Customer},
	    shop(Waiting, Barber);
	close ->
	    Waiting ! close,
	    Barber ! close,
	    ok
    end.
	    
    

    
