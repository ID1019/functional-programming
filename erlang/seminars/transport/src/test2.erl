-module(test2).

-export([start/2]).

start(Id, Hub) ->
    spawn(fun() -> init(Id, Hub) end).


init(Id, Hub) ->		  
    {ok, Net} = network:start(self(), Id), 
    {ok, Link}  = link:start(Net),
    Net ! {connect, Link},
    Hub ! {connect, Link},
    Link ! {connect, Hub},
    node(Id, Net).

node(Id, Net) ->
    receive
	{send, To, Msg} ->
	    Net ! {send, To, Msg},
	    node(Id, Net);
	Msg ->
	    io:format("node: ~w received ~w~n", [Id, Msg]),
	    node(Id, Net)
    end.	    
    


    
    
    
    
