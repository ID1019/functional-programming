-module(nub).

-export([start/0, start/1]).

-include("frame.hrl").

start() ->
    {ok, spawn(fun() -> init(0) end)}.

start(Loss) ->
    %% Loss rate is given as percent of messges lost 0-100
    {ok, spawn(fun() -> init(Loss) end)}.

init(Loss) ->
    hub(Loss, []).

hub(Loss, Connected) ->
    receive 
	{connect, Pid} ->
	    io:format("connecting to  ~w~n", [Pid]),
	    Ref = erlang:monitor(process, Pid),
	    hub(Loss, [{Ref, Pid}|Connected]);

	{disconnect, Pid} ->
	    erlang:demonitor(process, Pid),
	    hub(Loss, lists:keydelete(Pid, 2, Connected));

	{'DOWN', Ref, process, _, _}  ->
	    io:format("died  ~w~n", [Ref]),
	    hub(Loss, lists:keydelete(Ref, 1, Connected));
	
	#frame{}=Frm ->
	    Lost = rand:uniform(100) =< Loss,
	    if 
		Lost ->
		    io:format("throwing away ~w~n", [Frm]),
		    ok;
		true ->
		    lists:foreach(fun({_,Pid}) -> Pid ! Frm end, Connected)
	    end,
	    hub(Loss, Connected);
	
	status ->
	    io:format("connect to  ~w~n", [Connected]),	    
	    hub(Loss, Connected);

	quit ->
	    ok

	end.
