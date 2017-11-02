%%% Given is a log of length N and a list of prices for logs of length
%%% l1, l2, ... The problem is to find the best possible way to cut
%%% the log to maximize profit.

-module(log).

-compile(export_all).

test(N) ->
    P = [{1,1}, {3,5}, {5,9}, {23,50}],
    cut(N, P).
    
%%% cut/ returns {Cuts, Rest, Price}

cut(0, _) ->
    {[], 0, 0};
cut(N, []) ->
    {[], N, 0};
cut(N, [{L,_}|Rest]) when N < L ->
    cut(N, Rest);
cut(N, [{L,P}|Rest]=Prices) when N >= L ->
    {C1, R1, P1} = cut(N-L, Prices),
    {C2, R2, P2} = cut(N, Rest),
    if 
	P1 + P > P2 ->
	    {[L|C1], R1, P1+P};
	true ->
	    {C2, R2, P2}
    end.	    
    

