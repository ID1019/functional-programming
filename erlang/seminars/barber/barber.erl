-module(barber).

-export([start/1]).

-define(Cut, 1000).

start(Waiting) ->
    {ok, spawn_link(fun() -> init(Waiting) end)}.

init(Waiting) ->
    barber(Waiting).

barber(Waiting) ->
    Waiting ! {next, self()},
    receive
	close ->
	    ok;
	{ok, Customer} ->
	    Customer ! have_a_seat,
	    timer:sleep(?Cut),
	    Customer ! howz_that,
	    barber(Waiting)
    end.




    
		        
