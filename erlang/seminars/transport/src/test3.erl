-module(test3).

-export([start/2]).

start(Id, Hub) ->
    spawn(fun() -> init(Id, Hub) end).


init(Id, Hub) ->		  
    {ok, Trp} = transport:start(self()),

    {ok, Net} = network:start(Trp, Id), 

    {ok, Link}  = link:start(Net),
    
    Trp ! {connect, Net},
    Net ! {connect, Link},
    Link ! {connect, Hub},

    Hub ! {connect, Link},
    node(Id, Trp).

node(Id, Trp) ->
    receive
	{send, To, Msg} ->
	    Trp ! {send, To, Msg},
	    node(Id, Trp);
	Msg ->
	    io:format("node: ~w received ~w~n", [Id, Msg]),
	    node(Id, Trp)
    end.	    
    

