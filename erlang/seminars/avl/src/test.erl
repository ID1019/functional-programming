-module(test).

-compile(export_all).


bench(N) ->
    Seq = sequence(N, 100*N),
    {Ad, Am} = bench(Seq, avl),
    io:format("AVL tree of max depth ~w and average depth ~.1f~n", [Am, Ad/N]),
    {Bd, Bm} = bench(Seq, bst),
    io:format("BST tree of max depth ~w and average depth ~.1f~n", [Bm, Bd/N]).


bench(Seq, Module) ->
    Empty = apply(Module, tree, []),
    Inserted = lists:foldl(fun(K, A) ->  apply(Module, insert, [A, K, K])  end,  Empty, Seq),
    Total = lists:foldl(fun(K, A) ->  {ok, D} = apply(Module, depth, [Inserted, K]), D+A end, 0, Seq),
    Max =  apply(Module, max_depth, [Inserted]),
    {Total, Max}.

time(N) ->
    Seq = sequence(N, 100*N),
    {A1, A2, Ad, Am} = bench(Seq, avl),
    io:format("AVL tree of max depth ~w and average depth ~.1f  - constructed in ~w us, lookup in ~w us~n", [Am, Ad/N, A1, A2]),
    {B1, B2, Bd, Bm} = bench(Seq, bst),
    io:format("BST tree of max depth ~w and average depth ~.1f  - constructed in ~w us, lookup in ~w us~n", [Bm, Bd/N, B1, B2]).

time(Seq, Module) ->
    Empty = apply(Module, tree, []),
    {T1, Inserted} = timer:tc(fun() -> 
			lists:foldl(fun(K, A) ->  apply(Module, insert, [A, K, K])  end,  Empty, Seq) end),    
    {T2, Total} = timer:tc(fun() -> lists:foldl(fun(K, A) ->  {ok, D} = apply(Module, depth, [Inserted, K]), D+A end, 0, Seq) end),
    Max =  apply(Module, max_depth, [Inserted]),
    {T1, T2, Total, Max}.


    

sequence(0,_) ->
    [];
sequence(I, T) ->
    [rand:uniform(T)|sequence(I-1, T)].






