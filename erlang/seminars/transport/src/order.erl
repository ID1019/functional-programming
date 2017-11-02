-module(order).

-export([start/2]).

-record(ack, {id=na}).
-record(dgr, {id=na, data=[]}).

start(Master, To) ->   {ok, spawn(fun() -> init(Master, To) end)}.

init(Master, To) ->
    receive
	{connect, Net} ->   order(Master, To, 0, 0, [], Net)
    end.

order(Master, To, N, I, [], Net) ->
    receive
	#dgr{id=I, data=Msg} ->
	    Net ! {send, To, #ack{id=I}},	    
	    Master ! Msg,
	    order(Master, To, N, I+1, [], Net);
	#dgr{id=J} when J < I ->
	    Net ! {send, To, #ack{id=J}},	    
	    order(Master, To, N, I, [], Net);
	#ack{} ->
	    order(Master, To, N, I, [], Net);
	{send, Msg} ->
	    Net ! {send, To, #dgr{id=N, data=Msg}},
	    order(Master, To, N+1, I, [{N,Msg}], Net);
	{master, New} ->
	    order(New, To, N, I, [], Net)
    end;
order(Master, To, N, I, [{A,Res}|Rest]=Buffer, Net) ->
    receive
	#dgr{id=I, data=Msg} ->
	    Net ! {send, To, #ack{id=I}},	    
	    Master ! Msg,
	    order(Master, To, N, I+1, Buffer, Net);
	#dgr{id=J} when J < I ->
	    Net ! {send, To, #ack{id=J}},	    
	    order(Master, To, N, I, Buffer, Net);
	#ack{id=A} ->
	    order(Master, To, N, I, Rest, Net);
	#ack{id=B} when B < A ->
	    order(Master, To, N, I, Buffer, Net);
	{send, Msg} ->
	    Net ! {send, To, #dgr{id=N, data=Msg}},
	    order(Master, To, N+1, I, Buffer++[{N,Msg}], Net);
	{master, New} ->
	    order(New, To, N, I, Buffer, Net)
		
    after 10 ->
	    Dgr = #dgr{id=A, data=Res},
	    io:format("order re-sending ~w~n", [Dgr]),
	    Net ! {send, To, Dgr},
	    order(Master, To, N, I, Buffer, Net)
    end.





    
    



	    
			  
	    
    
