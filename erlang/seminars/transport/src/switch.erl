-module(switch).

-record(msg, {src=0, dst=0, data=[]}).

-export([start/0, new/1]).

start() ->
    {ok, spawn(fun() -> init() end)}.


new(Switch) ->
    Switch ! {new, self()},
    receive 
	{ok, Lnk} ->
	    {ok, Lnk}
    end.
    


init() ->
    switch([], []).

%% Hmm, how do we model a switch? It should learn from the incomming
%% packets. Hmm, ...

switch(Cache, All) ->
    receive 
	{new, Net} ->
	    Self = self(),
	    Con = spawn(fun()-> connection(Self) end),
	    {ok, Link} = link:start(Con),
	    Con ! {connect, Link},
	    Net ! {ok, Link},
	    io:format("switch new link: ~w~n", [Link]),
	    switch(Cache, [Con|All]);
	{frw, Src, Dst, From, Msg} ->
	    case lists:keyfind(Dst, 1, Cache) of
		{Dst, To} ->
		    io:format("switch forward msg : ~w~n", [Msg]),
		    To ! {frw, Msg};
		false ->
		    io:format("switch broadcast msg : ~w~n", [Msg]),
		    lists:foreach(fun(C) -> C ! {frw, Msg} end, All)
	    end,
	    Rest = lists:keydelete(Src,1,Cache),
	    switch([{Src, From}|Rest], All);
	status ->
	    io:format("switch cache: ~w~n", [Cache]),
	    switch(Cache, All)
    end.
 
connection(Switch) ->
    receive
	{connect, Lnk} ->
	    io:format("switch connection connected to: ~w~n", [Lnk]),
	    connection(Switch, Lnk)
    end.
		  
connection(Switch, Lnk) ->
    receive
	#msg{src=Src, dst=Dst}=Msg ->
	    %%io:format("switch connection received: ~w~n", [Msg]),
	    Switch ! {frw, Src, Dst, self(), Msg},
	    connection(Switch, Lnk);
	{frw, Msg} ->
	    %%io:format("switch connection deliver: ~w~n", [Msg]),
	    Lnk ! {send, Msg},
	    connection(Switch, Lnk)	    
    end.
    
	    
		
    
