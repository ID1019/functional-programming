-mode(node3).

-export([start/2]).


init(Id, Hub) ->		  
    {ok, Tcp} = transport:start(self())
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
    


