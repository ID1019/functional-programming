-module(hub).

-export([start/0]).

-include("frame.hrl").

start() ->
    {ok, spawn(fun() -> init() end)}.

init() ->
    hub([]).

hub(Connected) ->
    receive 
	{connect, Pid} ->
	    Ref = erlang:monitor(process, Pid),
	    hub([{Ref, Pid}|Connected]);

	{disconnect, Pid} ->
	    erlang:demonitor(process, Pid),
	    hub(lists:keydelete(Pid, 2, Connected));

	{'DOWN', Ref, process, _, _}  ->
	    hub(lists:keydelete(Ref, 1, Connected));
	
	#frame{}=Frm ->
	    lists:foreach(fun({_,Pid}) -> Pid ! Frm end, Connected),
	    hub(Connected);
	
	status ->
	    io:format("connect to ~w~n", [Connected]),
	    hub(Connected);	    

	quit ->
	    ok

	end.
