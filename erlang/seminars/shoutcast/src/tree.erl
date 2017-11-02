-module(tree).

-compile(export_all).

new() ->
    seed.


add(V, seed) ->
    {node, V, seed, seed};
add(V, {node, M, L, R}) when V < M ->
    {node, M, add(V, L), R};
add(V, {node, M, L, R})  ->
    {node, M, L, add(V, R)}.    


sum(seed) ->      
    0;
sum({node, V, Left, Right}) -> 
    V + sum(Left)+ sum(Right).

lookup(_, seed) ->    
    false;
lookup(V, {node, V, _, _}) ->
    true;
lookup(V, {node, M, L, _}) when V < M ->
    lookup(V, L);
lookup(V, {node, _, _, R}) ->
    lookup(V, R).
