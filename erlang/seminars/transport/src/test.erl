-module(test).

-compile(export_all).

test_flow(Loss) ->
    Sender = spawn(fun() -> connect(fun(N) ->  sender_flow(20, N) end) end),
    Receiver = spawn(fun() ->  connect(fun(N) -> receiver_flow(20, N) end) end),
    {ok, Ls} = flow(Sender, 4, 2, 1),
    {ok, Lr} = flow(Receiver, 4, 1, 2),
    connect(Ls, Lr, Loss).

test_flow() ->
    Sender = spawn(fun() -> connect(fun(N) ->  sender_flow(20, N) end) end),
    Receiver = spawn(fun() ->  connect(fun(N) -> receiver_flow(20, N) end) end),
    {ok, Ls} = flow(Sender, 4, 2, 1),
    {ok, Lr} = flow(Receiver, 4, 1, 2),
    switch(Ls, Lr).


test_order() ->
    Sender =  spawn(fun() -> connect(fun(N) -> sender_order(20, N) end) end),
    Receiver = spawn(fun() ->  connect(fun(N) -> receiver_order(20, N) end) end),
    {ok, Ls} = order(Sender, 2, 1),
    {ok, Lr} = order(Receiver, 1, 2),
    switch(Ls, Lr).        

test_order(Loss) ->
    Sender =  spawn(fun() -> connect(fun(N) -> sender_order(20, N) end) end),
    Receiver = spawn(fun() ->  connect(fun(N) -> receiver_order(20, N) end) end),
    {ok, Ls} = order(Sender, 2, 1),
    {ok, Lr} = order(Receiver, 1, 2),
    connect(Ls, Lr, Loss).    

test_switch() ->
    Sender =  spawn(fun() -> connect(fun(N) -> sender_netw(20, 2, N) end) end),
    Receiver =  spawn(fun() -> connect(fun(N) -> receiver_netw(20, 1, N) end) end),
    {ok, Ls} = netw(Sender, 1),
    {ok, Lr} = netw(Receiver, 2),
    switch(Ls, Lr).        


test_netw(Loss) ->
    Sender =  spawn(fun() -> 
		 connect(fun(N) -> 
		       sender_netw(20, 2, N) end) end),
    Receiver =  spawn(fun() -> 
                   connect(fun(N) -> 
                       receiver_netw(20, 1, N) end) end),
    {ok, Ls} = netw(Sender, 1),
    {ok, Lr} = netw(Receiver, 2),
    connect(Ls, Lr, Loss).    

flow(App, Size, To, I) ->
    {ok, Flow} = flow:start(Size),    
    App ! {connect, Flow},
    order(Flow, To, I).

order(App, To, I) ->
    {ok, Order} = order:start(App, To),    
    App ! {connect, Order},
    netw(Order, I).

netw(App, I) ->    
    {ok, Netw} = network:start(App, I),
    App ! {connect, Netw},
    lnk(Netw).

lnk(App) ->
    {ok, Link} = link:start(App),
    App ! {connect, Link},
    {ok, Link}.

switch(L1, L2) ->
    {ok, Switch} = switch:start(),
    {ok, S1} = switch:new(Switch),
    {ok, S2} = switch:new(Switch),
    L1 ! {connect, S1},
    S1 ! {connect, L1},    
    L2 ! {connect, S2},
    S2 ! {connect, L2}.
     


connect(L1, L2, Loss) ->
    {ok, Hub} = nub:start(Loss),
    L1 ! {connect, Hub},
    L2 ! {connect, Hub},
    Hub ! {connect, L1},
    Hub ! {connect, L2}.

connect(F) ->
    receive
	{connect, N} ->
	    F(N)
    end.

sender_flow(0, _) ->
    ok;
sender_flow(I, N) ->
    io:format("sender sending ~w~n", [I]),
    N ! {send, I, self()},
    receive 
	ok ->
	    sender_flow(I-1, N)
    end.

receiver_flow(0,_) ->
    ok;
receiver_flow(I,N) ->
    N ! {read, 4, self()},	    
    receive
	{ok, L, Msg} ->
	    io:format("receiver received ~w~n", [Msg]),
	    receiver_flow(I-L,N)
    end.

sender_order(0, _) ->
    ok;
sender_order(I, N) ->
    io:format("sender sending ~w~n", [I]),
    N ! {send, I},
    sender_order(I-1, N).

receiver_order(0,_) ->
    ok;
receiver_order(I,N) ->
    receive
	Msg ->
	    io:format("receiver received ~w~n", [Msg]),
	    receiver_order(I-1,N)
    end.


sender_netw(0, _, _) ->
    ok;
sender_netw(I, T, N) ->
    io:format("sender sending ~w~n", [I]),
    N ! {send, T, I},
    sender_netw(I-1, T, N).

receiver_netw(0, _, _) ->
    ok;
receiver_netw(I, T, N) ->
    receive
	Msg ->
	    io:format("receiver received ~w~n", [Msg]),
	    receiver_netw(I-1, T, N)
    end.
