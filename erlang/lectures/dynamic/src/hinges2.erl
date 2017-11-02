%%% The task is to maximize profit from producing hinges (gångjärn)
%%% and latches (skjutlås) given a set of requirements. We have a
%%% maximum amount of raw material and maximum time that we can
%%% use. The hinges and latches need different amount of resources and
%%% are sold at diferent proces. Find out how many hinges and latches
%%% taht one should produce in order to maximize profit.


-module(hinges2).

-compile(export_all).

			
%%% search/4 the first item is either a hinge or a latch. First
%%% compute the maximum profit if staring with a hinge and then do the
%%% same for a latch and take the maximum of the two answers.

search(M, T, {Hm, Ht, Hp}=H, {Lm, Lt, Lp}=L) when  (M >= Hm) and (T >= Ht) and (M >= Lm) and (T >= Lt)->
    {Hi, Li, Pi} = search((M-Hm), (T-Ht), H, L), 
    {Hj, Lj, Pj} = search((M-Lm), (T-Lt), H, L), 
    if 
	(Pi+Hp) > (Pj+Lp) ->
	    {(Hi+1), Li, (Pi+Hp)};
	true ->
	    {Hj, (Lj+1), (Pj+Lp)}
    end;
search(M, T, {Hm, Ht, Hp}=H, L) when ((M >= Hm) and (T >= Ht))  ->
    {Hn, Ln, P} = search((M-Hm), (T-Ht), H, L),
    {Hn+1, Ln, (P+Hp)};
search(M, T, H, {Lm, Lt, Lp}=L) when ((M >= Lm) and (T >= Lt))  ->
    {Hn, Ln, P} = search((M-Lm), (T-Lt), H, L),
    {Hn, Ln+1, P+Lp};
search(_, _, _, _)  ->
    {0,0,0}.


%%% The search/4 solution can be extended with a memory and then
%%% bring the compexity down to O(n^2).

memory(M, T, H, L) ->
    Mem = new(),
    {Hn, Ln, P, _} = search(M, T, H, L, Mem),
    {Hn, Ln, P}.

check(M, T, H, L, Mem) ->
    case lookup({M,T}, Mem) of
	false ->
	    {Solution, Upd} = search(M, T, H, L, Mem),
	    {Solution, store({M,T}, Solution, Upd)};
	 {_, {Mh, Ml, Mp}} -> 
	    {Mh, Ml, Mp, Mem}
    end.
    
search(M, T, {Hm, Ht, Hp}=H, {Lm, Lt, Lp}=L, Mem) when (M >= Hm) and (T >= Ht) and (M >= Lm) and (T >= Lt) ->
    {Hi, Li, Pi, Upd1} = check((M-Hm), (T-Ht), H, L, Mem), 
    {Hj, Lj, Pj, Upd2} = check((M-Lm), (T-Lt), H, L, Upd1), 
    if 
	(Pi+Hp) > (Pj+Lp) ->
	    {{(Hi+1), Li, (Pi+Hp)}, Upd2};
	true ->
	    {{Hj, (Lj+1), (Pj+Lp)}, Upd2}
    end;
search(M, T, {Hm, Ht, Hp}=H, L, Mem) when ((M >= Hm) and (T >= Ht))  ->
    {Hn, Ln, P, Upd} = check((M-Hm), (T-Ht), H, L, Mem),
    {{Hn+1, Ln, (P+Hp)}, Upd};
search(M, T, H, {Lm, Lt, Lp}=L, Mem) when ((M >= Lm) and (T >= Lt)) ->
    {Hn, Ln, P, Upd} = check((M-Lm), (T-Lt), H, L, Mem),
    {{Hn, Ln+1, P+Lp}, Upd};
search(_, _, _,  _, Mem)  ->
    {{0,0,0}, Mem}.

new() ->
    [].
    
store(K,V, Mem) ->
     [{K,V}|Mem].
    
lookup(K, Mem) ->
     lists:keyfind(K, 1, Mem).


%%% The memory/4 solution but now with a tree memory.

tree(M, T, H, L) ->
    Mem = new_tree(),
    {Hn, Ln, P, _} = search_tree(M, T, H, L, Mem),
    {Hn, Ln, P}.

check_tree(M, T, H, L, Mem) ->
    case lookup_tree({M,T}, Mem) of
	false ->
	    search_tree(M, T, H, L, Mem);
	 {_, {Mh, Ml, Mp}} -> 
	    {Mh, Ml, Mp, Mem}
    end.
    
search_tree(M, T, {Hm, Ht, Hp}=H, {Lm, Lt, Lp}=L, Mem) when (M >= Hm) and (T >= Ht) and (M >= Lm) and (T >= Lt) ->
    {Hi, Li, Pi, Upd1} = check_tree((M-Hm), (T-Ht), H, L, Mem), 
    {Hj, Lj, Pj, Upd2} = check_tree((M-Lm), (T-Lt), H, L, Upd1), 
    if 
	(Pi+Hp) > (Pj+Lp) ->
	    {(Hi+1), Li, (Pi+Hp), store_tree({M,T}, {(Hi+1), Li, (Pi+Hp)}, Upd2)};
	true ->
	    {Hj, (Lj+1), (Pj+Lp), store_tree({M,T}, {Hj, (Lj+1), (Pj+Lp)}, Upd2)}
    end;
search_tree(M, T, {Hm, Ht, Hp}=H, L, Mem) when ((M >= Hm) and (T >= Ht))  ->
    {Hn, Ln, P, Upd} = check_tree((M-Hm), (T-Ht), H, L, Mem),
    {Hn+1, Ln, (P+Hp), store_tree({M,T}, {Hn+1, Ln, (P+Hp)}, Upd)};
search_tree(M, T, H, {Lm, Lt, Lp}=L, Mem) when ((M >= Lm) and (T >= Lt)) ->
    {Hn, Ln, P, Upd} = check_tree((M-Lm), (T-Lt), H, L, Mem),
    {Hn, Ln+1, P+Lp,  store_tree({M,T}, {Hn, Ln+1, P+Lp}, Upd)};
search_tree(M, T, _,  _, Mem)  ->
    {0,0,0, store_tree({M,T}, {0,0,0}, Mem)}.

new_tree() ->
    gb_trees:empty().
    
store_tree(K,V, Mem) ->
    gb_trees:insert(K, V, Mem).
    
lookup_tree(K, Mem) ->
    case gb_trees:lookup(K, Mem) of none ->  false; {value, Value} ->  {K, Value}  end.


%%% In the ordered/4 version we only check sequences that start with
%%% hinges and continues with latches. This brings the computation
%%% time down to O(n^2) same as the easy one.

ordered(M, T, {Hm, Ht, _}, L) when (M < Hm) or (T < Ht) ->
    {N, P} = then_latches(M, T, L),
    {0, N, P};
ordered(M, T, {Hm, Ht, Hp}=H, L) ->
    {Hi, Li, Pi} = ordered((M-Hm), (T-Ht), H, L), 
    Latches = then_latches(M, T, L), 
    {Lj, Pj} = Latches,
    if 
	(Pi+Hp) > Pj ->
	    {(Hi+1), Li, (Pi+Hp)};
	true ->
	    {0, Lj, Pj}
    end.


then_latches(M, T, {Lm, Lt, _}) when (M < Lm) or (T < Lt) ->
    {0, 0};
then_latches(M, T, {Lm, Lt, Lp}=L) ->
    {N, P} = then_latches((M-Lm), (T-Lt), L),
    {N+1, P+Lp}.


%%% Note that for this problem there is no point in trying to save
%%% answers since the computation does not recompute any points
%%% (unless the hinges and latches have the same parameters). If we
%%% had three items the computations would make duplicates and it
%%% could be profitable to store computetd values.




%%% The easy way is to simple generate all possible combinations of
%%% the number of hinges and latches and then check 1/ is it possible
%%% to make them given The resources and 2/ what the eprofit would
%%% be. The algorithm is O(n^2) so it's ok complexity wise. The
%%% solution relies on the fact that we can compute a finite set of
%%% possible solutions and then iterate over these.

easy(M, T, {Hm, Ht, Hp}, {Lm, Lt, Lp}) ->
    MaxH = max(trunc(M/Hm), trunc(T/Ht)),
    MaxL = max( trunc(M/Lm), trunc(T/Lt)),
    All = [{H,L} || H <- lists:seq(1,MaxH), L <- lists:seq(1,MaxL)],
    Possible = lists:filter(fun({H,L}) ->
				    ((H*Hm + L*Lm) =< M) and ((H*Ht + L*Lt) =< T) end, All),
    Profit = lists:map(fun({H, L}) ->
			       {H, L, (H*Hp + L*Lp)} end, Possible),
    lists:foldl(fun({H, L, P}, {_, _, Pa}=Acc) ->
			if
			    P > Pa ->
				{H, L, P};
			    true ->
				Acc
			end
		end, {0,0,0}, Profit).



bench1(N) ->
    All = lists:map(fun(X) -> {X * 1000, X * 200} end, lists:seq(1, N)),
    {ok, Fd} = file:open("memory.dat", [write]),
    io:format(Fd, "%~9s ~10s ~10s ~10s~n", ["M", "T", "search", "memory"]),
    lists:foreach(fun({M,T}) ->
			  {T1, _} = timer:tc(fun() -> search(M, T, {260, 40, 30}, {180, 60, 24}) end),
			  {T2, _} = timer:tc(fun() -> memory(M, T, {260, 40, 30}, {180, 60, 24}) end),
			  io:format(Fd, "~10w ~10w ~10w ~10w~n", [M, T, trunc(T1/1000), trunc(T2/1000)])
		  end,
		  All),
    file:close(Fd).



bench2(N) ->
    All = lists:map(fun(X) -> {X * 10000, X * 2000} end, lists:seq(1, N)),
    {ok, Fd} = file:open("tree.dat", [write]),
    io:format(Fd, "%~9s ~10s ~10s ~10s~n", ["M", "T", "memory", "tree"]),
    lists:foreach(fun({M,T}) ->
			  {T1, _} = timer:tc(fun() -> memory(M, T, {260, 40, 30}, {180, 60, 24}) end),
			  erlang:garbage_collect(),
			  {T2, _} = timer:tc(fun() -> tree(M, T, {260, 40, 30}, {180, 60, 24}) end),
			  io:format(Fd, "~10w ~10w ~10w ~10w~n", [M, T, trunc(T1/1000), trunc(T2/1000)])
		  end,
		  All),
    file:close(Fd).


bench3(N) ->
    All = lists:map(fun(X) -> {trunc(30000*math:pow(1.5,X)), trunc(6000*math:pow(1.5,X))} end, lists:seq(1,N)),
    {ok, Fd} = file:open("ordered.dat", [write]),
    io:format(Fd, "%~9s ~10s ~10s ~10s~n", ["M", "T", "tree", "ordered"]),
    lists:foreach(fun({M,T}) ->
			  erlang:garbage_collect(),
			  {T1, _} = timer:tc(fun() -> tree(M, T, {260, 40, 30}, {180, 60, 24}) end),
			  erlang:garbage_collect(),
			  {T2, _} = timer:tc(fun() -> ordered(M, T, {260, 40, 30}, {180, 60, 24}) end),
			  io:format(Fd, "~10w ~10w ~10w ~10w~n", [M, T, trunc(T1/1000), trunc(T2/1000)])
		  end,
		  All),
    file:close(Fd).

						     

    
	       
