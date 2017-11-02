-module(flow).

-record(msg, {data=[]}).
-record(syn, {add=0}).

-export([start/1]).

start(Size) ->
    {ok, spawn(fun() -> init(Size) end)}.

init(Size) ->
    receive
	{connect, Net} ->
	    io:format("flow connecting to ~w~n", [Net]),
	    Net ! {send, #syn{add=Size}},
	    flow(Size, 0, [], Net)
    end.
    
flow(S, 0, Buffer, Net) ->
    receive
	#syn{add=T} ->
	    flow(S, T, Buffer, Net)
    end;
flow(S, T, [], Net) ->
    receive 
	{send, Msg, Pid} ->
	    Net ! {send, #msg{data=Msg}},
	    Pid ! ok,
	    flow(S, T-1, [], Net);
	#msg{data=Msg} ->
	    flow(S-1, T, [Msg], Net);
	#syn{add=A} ->
	    flow(S, T+A, [], Net)
    end;
flow(S, T, Buffer, Net) ->
    receive 
	{send, Msg, Pid} ->
	    Net ! {send, #msg{data=Msg}},
	    Pid ! ok,	    
	    flow(S, T-1, Buffer, Net);
	{read, N, Pid} ->
	    {I, Deliver, Rest} = read(N, Buffer),
	    Pid ! {ok, I, Deliver},
	    Net ! {send, #syn{add=I}},
	    flow(S+I, T, Rest, Net);
	#msg{data=Msg} ->
	    flow(S-1, T, Buffer++[Msg], Net);
	#syn{add=A} ->
	    flow(S, T+A, Buffer, Net)
    end.



read(N, Buffer) ->
    L = length(Buffer),
    if N =< L ->
	    {Deliver, Keep} = lists:split(N, Buffer),
	    {N, Deliver, Keep};
       true ->
	    {L, Buffer, []}
    end.
