-module(network).

-record(msg, {src=0, dst=0, data=[]}).

-export([start/2]).

start(Master, Id) ->
    Con = spawn(fun() -> init(Master, Id) end),
    {ok, Con}.

init(Master, Id) ->
    receive 
	{connect, Link} ->
	    io:format("network layer ~w connected~n", [Id]),	    
	    network(Master, Id, Link);
	quit ->
	    ok
    end.

network(Master, Id, Link) ->
    receive
	{send, To, Msg} ->
	    io:format("network layer ~w sending ~w~n", [Id, Msg]),
	    Link ! {send, #msg{src=Id, dst=To,  data=Msg}},
	    network(Master, Id, Link);

	#msg{dst=Id, data=Msg} ->
	    io:format("network layer ~w received ~w~n", [Id, Msg]),
	    Master ! Msg,
	    network(Master, Id, Link);

	#msg{}=_ ->
	    network(Master, Id, Link);

	{master, New} ->
	    network(New, Id, Link);	    

	status -> 
	    io:format("master: ~w id: ~w  link: ~w~n", [Master, Id, Link]),
	    network(Master, Id, Link);	    

	quit ->
	    ok
    end.

	    
			
    







