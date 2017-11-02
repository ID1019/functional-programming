-module(high).

-compile(export_all).

foldr(_Op, Acc, []) ->
   Acc;
foldr(Op, Acc, [H|T]) ->
   Op(H, foldr(Op, Acc, T)).

foldl(_Op, Acc, []) ->
   Acc;
foldl(Op, Acc, [H|T]) ->
   foldl(Op, Op(H, Acc), T).






appendl(L) ->
    F = fun(E,A) -> A ++ E end,
    foldl(F, [], L).

appendr(L) ->
    F = fun(E,A) -> E ++ A end,
    foldr(F, [], L).










qsort([]) -> [];
qsort([P|L]) ->
    Lower = [X||X <- L, X < P],
    Higher = [X||X <- L, X >= P],
    Lower ++ [P|Higher].
