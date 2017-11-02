-module(bench).

-compile(export_all).


tree() ->
    R = 100000,
    io:format("# Benchmark of tree operations, (~w runs), times in ms~n#~n", [R]),
    io:format("# n * 1000 is size of tree~n", []),
    io:format("#~7s ~8s ~8s ~8s ~8s~n", ["n", "insert", "lookup", "modify", "delete"]),

    Ns = [1000,2000,4000,8000,16000,32000,64000,128000,256000,512000,1024000],  

    Bench = fun(N) ->
       Tree = init_tree(N),		    
       Ops = init_ops(R,N),
       Ti  = time(fun() -> lists:foreach(fun(S) -> tree:insert(S, na, Tree) end, Ops) end),
       Tl  = time(fun() -> lists:foreach(fun(S) -> tree:lookup(S, Tree)     end, Ops) end),
       Tm  = time(fun() -> lists:foreach(fun(S) -> tree:modify(S, na, Tree) end, Ops) end),
       Td  = time(fun() -> lists:foreach(fun(S) -> tree:delete(S, Tree)     end, Ops) end),
       io:format("~8w ~8w ~8w ~8w ~8w~n", [N div 1000, Ti, Tl, Tm, Td])
       end,
    lists:foreach(Bench, Ns).



tuple() ->
    R = 100000,
    io:format("# Benchmark of tuple operations, (~w runs), times in ms~n#~n", [R]),
    io:format("# n * 1000 is size of tuple~n", []),
    io:format("#~7s ~8s ~8s ~8s ~8s~n", ["n", "insert", "lookup", "modify", "delete"]),
    Ns = [1000,2000,4000,8000,16000,32000],

    Bench = fun(N) ->
       Table = init_table(N),
       Ops = init_ops(R,N),
       Ti  = time(fun() ->  lists:foreach(fun(S) -> tuple:insert(S, foo, Table) end, Ops) end),
       Tl  = time(fun() ->  lists:foreach(fun(S) -> tuple:lookup(S, Table)      end, Ops) end),
       Tm  = time(fun() ->  lists:foreach(fun(S) -> tuple:modify(S, foo, Table) end, Ops) end),
       Td  = time(fun() ->  lists:foreach(fun(S) -> tuple:delete(S, Table)      end, Ops) end),
       io:format("~8w ~8w ~8w ~8w ~8w~n", [N div 1000, Ti, Tl, Tm, Td])
       end,
    lists:foreach(Bench, Ns).

combined() ->
    R = 100000,
    io:format("# Benchmark of tree vs tuple, modify operations, (~w runs), times in ms~n#~n", [R]),
    io:format("# n  is is size of tree/tuple~n", []),
    io:format("#~7s ~8s ~8s ~n", ["n", "tree", "tuple"]),
    Ns = [100,200,400,800,1600,3200],
    Bench = fun(N) ->
       %% create a list of N random number
       Table = init_table(N),
       Tree = init_tree(N),		    
       Ops = init_ops(R,N),
       Tr  = time(fun() -> lists:foreach(fun(S) -> tree:modify(S, na, Tree) end, Ops) end),
       Tu  = time(fun() ->  lists:foreach(fun(S) -> tuple:modify(S, foo, Table) end, Ops) end),
       io:format("~8w ~8w ~8w ~n", [N, Tr, Tu])
       end,
    lists:foreach(Bench, Ns).
init_table(N) ->
    list_to_tuple(lists:seq(1,N)).
    
init_tree(N) ->
    Seq = random(N,N),
    Empty = tree:empty(),
    lists:foldl(fun(S, T) -> tree:insert(S, na, T) end, Empty, Seq).

init_ops(R,N) ->
    random(R,N).

random(0,_) ->
    [];
random(R,N) ->
    [random:uniform(N) | random(R-1, N)].

time(F)->
    %% time in micro seconds
    T1 = erlang:system_time(milli_seconds),
    F(),
    T2 = erlang:system_time(milli_seconds),
    T2 - T1.



			     

    


