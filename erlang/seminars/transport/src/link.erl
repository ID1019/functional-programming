-module(link).

-include("frame.hrl").

-export([start/1]).

start(Master) ->
    Lnk = spawn(fun() -> init(Master) end),
    {ok, Lnk}.

init(Master) ->
    receive 
	{connect, Lnk} ->
	    link(Master, Lnk);
	quit ->
	    ok
    end.

link(Master, Lnk) ->
    receive 
	{send, Msg} ->
	    Lnk ! #frame{data=Msg},
	    link(Master, Lnk);
	#frame{data=Msg} ->
	    Master ! Msg,
	    link(Master, Lnk);

	{master, New} ->
	    link(New, Lnk);

	status ->
	    io:format("master: ~w  link: ~w~n", [Master, Lnk]),
	    link(Master, Lnk);


	quit ->
	    ok
    end.
