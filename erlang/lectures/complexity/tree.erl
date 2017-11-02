-module(tree).

-export([empty/0, lookup/2, modify/3, insert/3, delete/2]).

empty() ->
    nil.

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
    

