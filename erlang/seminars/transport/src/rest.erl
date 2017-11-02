-module(test).

-compile(export_all).

test(Loss) ->
    {ok, Ls} = sender(),
    {ok, Lr} = receiver(),
    connect(Ls, Lr, Loss),

sender(Size, To, I) ->
    Sender = spawn(fun() -> sender_syn(20) end),
    flow(Sender, Size, To, I).

receiver(Size, To, I) ->  
    Sender = spawn(fun() -> receiver_syn(20) end),
    flow(Seder, Size, To, I).

flow(App, Size, To, I) ->
    {ok, Flow} = flow:start(Size),    
    App ! {connect, Flow},
    order(Flow, To, I).

order(Flow, To, I) ->
    {ok, Order} = order:start(Flow, To),    
    netw(Order, I).

netw(Order, I) ->    
    {ok, Netw} = network:start(Order, I),
    lnk(Netw).

lnk(Netw) ->
    {ok, Link} = link:start(Netw),
    Netw ! {connect, Link},
    {ok, Link}.

conect(Ls, Lr, Loss) ->
        {ok, Hub} = nub:start(Loss),
        Ls ! {connect, Hub},
        Lr ! {connect, Hub},
        Hub ! {connect, Ls},
        Hub ! {connect, Lr}.



sender_syn(I) ->
    receive
	{connect, N} ->
	    sender_syn(I, N)
    end.

receiver_syn(I) ->
    receive
	{connect, N} ->
	    receiver_syn(I, N)
    end.



sender_syn(0, _) ->
    ok;
sender_syn(I, N) ->
    N ! {send, I, self()},
    receive 
	ok ->
	    sender_syn(I-1, N)
    end.


receiver_syn(0,_) ->
    ok;
receiver_syn(I,N) ->
    N ! {read, 4, self()},	    
    receive
	{ok, L, Msg} ->
	    io:format("receiver received ~w ~w~n", [I, Msg]),
	    receiver_syn(I-L,N)
    end.
