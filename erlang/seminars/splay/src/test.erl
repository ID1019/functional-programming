-module(test).

-compile(export_all).


test() ->
    Insert = [7,2,5,3,9,4,8,1,0,4,3,5,4,6,5,4,8,2,3,7,8,5,4,3,6],
    Tree = splay:tree(),
    Updated = lists:foldl(fun(K, A) -> splay:update(A, K, K) end, Tree, Insert),
    lists:foldl(fun(K, A) -> {ok, K, U} = splay:lookup(A,K), U end, Updated, Insert),
    {ok, 9} = ordered(Updated, -1),
    traverse(Updated).			
       

test(Insert) ->
    Tree = splay:tree(),
    Updated = lists:foldl(fun(K, A) -> splay:update(A, K, K) end, Tree, Insert),
    lists:foldl(fun(K, A) -> {ok, K, U} = splay:lookup(A,K), U end, Updated, Insert),
    {ok, 9} = ordered(Updated, -1),
    traverse(Updated).


ordered(nil, S) ->
    {ok, S};
ordered({node, K, _, L, R}, S) ->
    case ordered(L, S) of
	{ok, S1} -> 
	    if 
		S1 < K ->
		    ordered(R, K);
		true ->
		    false
	    end;
	false ->
	    false
    end.

traverse(T) ->
    traverse(T, []).

traverse(nil, S) ->
    S;
traverse({node, K, _, L, R}, S) ->
    traverse(L, [K| traverse(R, S)]).

