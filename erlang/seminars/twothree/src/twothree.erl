-module(twothree).

-compile(export_all).

%% An implementation of a  2-3 tree 


test() ->
    insertf(14, grk, {two, 7,
		      {three, 2, 5,
                       {leaf, 2, foo},
                       {leaf, 5, bar},
                       {leaf, 7, zot}},
                     {three, 13, 16,
                       {leaf, 13, foo},
                       {leaf, 16, bar},
                       {leaf, 18, zot}}}).



debug(0) ->
    ok;
debug(N) ->
    {Path, T} = debug(1024,nil),
    D = depth(T),
    if
	D =< 10  ->
	    debug(N-1);
	true ->
	    {D, Path}
    end.

test(0, T) -> T;
test(N, T) ->
    test(N-1, insert(random:uniform(1000), foo, T)).

debug(0,T) ->
    {[], T};
debug(N, T) ->
    R = random:uniform(1000),
    {Path, Tree} = debug(N-1, insert(R, foo, T)),
    {[R|Path], Tree}.

path([], T) ->
    T;
path([R|Path], T) ->
    path(Path, insert(R, foo, T)).

traverse({leaf, K, _}) ->
    [K];
traverse({two, _, L, R}) ->
    traverse(L) ++ traverse(R);
traverse({three, _, _, L, M, R}) ->
    traverse(L) ++ traverse(M) ++ traverse(R).
    
depth({leaf,_,_}) ->
    0;
depth({two, _, L,R}) ->
    1 + max(depth(L), depth(R));
depth({three, _, _,L,M,R}) ->
    1 + max(depth(L), max(depth(M), depth(R))).




insert(K, V, Root) ->
    case insertf(K, V, Root) of
	{four,Q1, Q2, Q3, T1, T2, T3, T4} ->
	    %% Special case for the root of the tree, never want to end up with a four node.
	    {two, Q2, {two, Q1, T1, T2}, {two, Q3, T3, T4}};
	Updated ->
	    Updated
    end.


insertf(K, V, nil) ->
    {leaf, K, V};

insertf(K, V, {leaf, K1, _}=L) ->
    if
	K =< K1 ->
	    {two, K, {leaf, K, V}, L};
	true ->
	    {two, K1, L, {leaf, K, V}}
    end;

insertf(K, V, {two, K1, {leaf, K1, _}=L1, {leaf, K2, _}=L2}) ->
    if 
	K =< K1 ->  
	    {three, K, K1, {leaf, K, V}, L1, L2};
	K =< K2 -> 
	    {three, K1, K, L1, {leaf, K, V}, L2};
	true ->
	    {three, K1, K2, L1, L2, {leaf, K, V}}
    end;

insertf(K, V, {three, K1, K2, {leaf, K1, _}=L1, {leaf, K2, _}=L2, {leaf, K3, _}=L3}) ->
    if 
	K =< K1 ->  
	    {four, K, K1, K2, {leaf, K, V}, L1, L2, L3};
	K =< K2 -> 
	    {four, K1, K, K2, L1, {leaf, K, V}, L2, L3};
	K =< K3 -> 
	    {four, K1, K2, K, L1, L2, {leaf, K, V}, L3};
	true ->
	    {four, K1, K2, K3, L1, L2, L3, {leaf, K, V}}
    end;
insertf(K, V, {two, K1, Left, Right}) ->
    if 
	K =< K1 ->
	    case insertf(K, V, Left) of
		{four, Q1, Q2, Q3, T1, T2, T3, T4} -> 
		    {three, Q2, K1, {two, Q1, T1, T2}, {two, Q3, T3, T4}, Right};
		Updated ->
		    {two, K1, Updated, Right}
	    end;
	true ->
	    case insertf(K, V, Right) of
		{four, Q1, Q2, Q3, T1, T2, T3, T4} -> 
		    {three, K1, Q2, Left, {two, Q1, T1, T2}, {two, Q3, T3, T4}};
		Updated ->
		    {two, K1, Left, Updated}
	    end
    end;
insertf(K, V, {three, K1, K2, Left, Middle, Right}) ->
    if 
	K =< K1 ->
	    case insertf(K, V, Left) of 
		{four, Q1, Q2, Q3, T1, T2, T3, T4} -> 
		    {four, Q2, K1, K2, {two, Q1, T1, T2}, {two, Q3, T3, T4}, Middle, Right}; 
		Updated ->
		    {three, K1, K2, Updated, Middle, Right}
	    end;
	K =< K2 ->
	    case insertf(K, V, Middle) of
		{four, Q1, Q2, Q3, T1, T2, T3, T4} -> 		
		    {four, K1, Q2, K2, Left, {two, Q1,T1, T2}, {two, Q3, T3, T4}, Right}; 
		Updated ->
		    {three, K1, K2, Left, Updated, Right}
	    end;
	true ->
	    case insertf(K, V, Right) of
		{four, Q1, Q2, Q3, T1, T2, T3, T4} -> 		
		    {four, K1, K2, Q2, Left, Middle, {two, Q1, T1, T2}, {two, Q3, T3, T4}}; 
		Updated ->
		    {three, K1, K2, Left, Middle, Updated}
	    end
    end.


    

     
