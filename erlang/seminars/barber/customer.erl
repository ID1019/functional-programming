-module(customer).

-export([start/2]).

-define(Walk, 1000).

start(Name, Shop) ->
    {ok, spawn_link(fun() -> init(Name, Shop) end)}.
		        
init(Name, Shop) ->
    io:format("~s - going for a hair cut~n", [Name]),
    customer(Name, Shop).

customer(Name, Shop) ->
    io:format("~s - enter the shop~n", [Name]),
    Shop ! {hello, self()},
    receive
	please_wait ->
	    io:format("~s - waiting~n", [Name]),
	    receive 
		have_a_seat ->
		    io:format("~s - taking a seat~n", [Name]), 
		    receive 
			howz_that ->
			    io:format("~s - thank you~n", [Name])
		    end
	    end;
	sorry ->
	    io:format("~s - taking a walk in the sun~n", [Name]),
	    timer:sleep(?Walk),
	    customer(Name, Shop)
    end.
