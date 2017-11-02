-module(bst).

-export([tree/0, insert/3, lookup/2, depth/2, max_depth/1]).

tree() ->
    nil.

insert(nil, Key, Value) ->
    %% empty tree
    {node, Key, Value, nil, nil};

insert({node, Key, _, A, B}, Key, Value) ->
    {node, Key, Value, A, B};

insert({node, Rk, Rv, A, B}, Kk, Kv)  when Kk < Rk ->
    {node, Rk, Rv, insert(A, Kk, Kv), B};
insert({node, Rk, Rv, A, B}, Kk, Kv)  when Kk > Rk ->
    {node, Rk, Rv, A, insert(B, Kk, Kv)}.



lookup(nil, _) ->
    fail;
lookup({node, K, V,  _, _}, K) ->
    {ok, V};
lookup({node, K, _,  L, R}, Key) ->
    if
	Key < K -> lookup(L, Key);
	true -> lookup(R, Key)
    end.


traverse(nil) ->
    {0,0};
traverse({node, _, _, L, R}) ->
    {Lt, Lm} = traverse(L),
    {Rt, Rm} = traverse(R),    
    {Lt+Rt+1, max(Lm, Rm)+1}.


depth(nil, _) ->
    fail;
depth({node, K, _, _, _}, K) ->
    {ok, 1};
depth({node, K, _, L, R}, Key)  ->
    if
	Key < K -> 
	    case depth(L, Key) of
		{ok, D} -> {ok, D + 1};
		fail -> fail
	    end;
	true -> 
	    case depth(R, Key) of
		{ok, D} -> {ok, D + 1};
		fail -> fail
	    end
    end.



max_depth(nil) ->
    0;
max_depth({node, _, _, L, R})  ->
    max(max_depth(L), max_depth(R)) + 1.






