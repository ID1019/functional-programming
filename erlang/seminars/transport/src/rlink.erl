-module(rlink).

-include("frame.hrl").

-export([start/1, encode/1, decode/1]).

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
	    Encoded = encode(Msg),
	    Lnk ! #frame{data=Encoded},
	    link(Master, Lnk);
	#frame{data=Data} ->
	    Msg = decode(Data),
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


encode([]) ->
    [];
encode([H|T]) ->
    encode(T, H, 1).

encode([], H, N) ->
    [H, N];
encode([H|T], H, N) ->
    encode(T, H, N+1);
encode([X|T], H, N) ->
    [H, N | encode(T, X, 1)].


decode([]) ->
    [];
decode([H, N|T]) ->
    decode(H, N, T).

decode(_, 0, T) ->
    decode(T);
decode(H, N, T) ->
    [H| decode(H, N-1, T)].







