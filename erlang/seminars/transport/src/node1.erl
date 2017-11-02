-module(node1).

-export([start/2]).

%% This node has a direct link to a hub. There is no network layer so
%% all messages that are sent to the hub will arrive at the node.

start(Id, Hub) ->
    {ok, spawn(fun() -> init(Id, Hub) end)}.

init(Id, Hub) ->		  
    {ok, Link}  = link:start(self()),
    Hub ! {connect, Link},
    Link ! {connect, Hub},
    node(Id, Link).

node(Id, Link) ->
    receive
	{send, Msg} ->
	    Link ! {send, Msg},
	    node(Id, Link);
	Msg ->
	    io:format("node: ~w received ~w~n", [Id, Msg]),
	    node(Id, Link)
    end.	    
    


    
    
    
    
