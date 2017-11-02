-module(tree2).

-compile(export_all).

lookup(_, nil) ->
    false;
lookup(K, {node, K, V, _, _}) -> 
    V;
lookup(K, {node, L, _, Left, Right}) -> 
    if 
       K < L ->  
           lookup(K, Left);
       true  -> 
           lookup(K, Right)
    end.


modify(_, _, nil) ->
    nil;
modify(K, V, {node, K, _, Left, Right}) -> 
    {node, K, V, Left, Right};
modify(K, V, {node, L, U, Left, Right}) -> 
    if 
       K < L ->  
           {node, L, U, modify(K, V, Left), Right};
       true  -> 
           {node, L, U,  Left, modify(K, V, Right)}
    end.

insert(K, V, nil) ->
    {node, K, V, nil, nil};
insert(K, V, {node, L, U, Left, Right}) -> 
    if 
       K < L ->  
           {node, L, U, insert(K, V, Left), Right};

       true  -> 
           {node, L, U,  Left, insert(K, V, Right)}
    end.

delete(_, nil) -> nil;
delete(K, {node, K, _, Node, nil}) ->  Node;
delete(K, {node, K, _, nil, Node}) ->  Node;
delete(K, {node, K, _, Left, Right}) -> 
      {U, L, Removed} = lift(Left),
      {node, U, L, Removed, Right};
delete(K, {node, U, L, Left, Right}) -> 
      if 
        K < U -> {node, U, L, delete(K, Left), Right};
        true -> {node, U, L, Left, delete(K, Right)}
    end.

lift({node, K, V, Left, nil}) ->  {K, V, Left};
lift({node, K, V, Left, Right}) ->  
    {U, L, Removed} = lift(Right),
    {U, L, {node, K, V, Left, Removed}}.
    


bench() ->
    N = 1000000,
    io:format("# Benchmark of tree opertations, (~w opertions), times in ms~n#~n", [N]),
    io:format("#~7s ~8s ~8s ~8s ~8s~n", ["n", "insert", "lookup", "modify", "delete"]),
  
    Ls = [32,64,128,256,512,1024],

    Bench = fun(L) ->
       %% create a list of L random numbers
       Seq = random(L,L),
       Empty = nil,
       Table = create_table(Seq, Empty),
       Bench = random(N,L),
       Ti  = time(fun() -> insert_all(Bench, Table) end),
       Tl  = time(fun() -> lookup_all(Bench, Table) end),
       Tm  = time(fun() -> modify_all(Bench, Table) end),
       Td  = time(fun() -> delete_all(Bench, Table) end),
       io:format("~8w ~8w ~8w ~8w ~8w~n", [L, round(Ti/1000), round(Tl/1000), round(Tm/1000), round(Td/1000)])
       end,
    lists:foreach(Bench, Ls).

random(0,_) ->
    [];
random(N,L) ->
    R = trunc(random:uniform() * L),
    [R | random(N-1, L)].


time(F)->
    %% time in micro seconds
    T1 = now(),
    F(),
    T2 = now(),
    timer:now_diff(T2, T1).


create_table(Seq, Table) ->
    lists:foldl(fun(S, T) -> insert(S, na, T) end, Table, Seq).    


insert_all(Seq, Table) ->
    lists:foreach(fun(S) -> insert(S, na, Table) end, Seq).

lookup_all(Seq, Table) ->
    lists:foreach(fun(S) -> lookup(S, Table) end, Seq).
			     
modify_all(Seq, Table) ->
    lists:foreach(fun(S) -> modify(S, na, Table) end, Seq).

delete_all(Seq, Table) ->
    lists:foreach(fun(S) -> delete(S, Table) end,  Seq).
    


