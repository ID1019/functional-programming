-module(root).

-compile(export_all).

-define(Guess, 12).

-define(Acc, 0.01).

sq(N) ->
    sq(N, ?Guess, ?Acc).


sq(N, G, A) ->
    io:format("guess: ~w ~n", [G]),
    case guess(N, G, A) of
	ok -> 
	    G;
	no ->
	    Next = (G + (N/G))/2,
	    sq(N, Next, A)
    end.


guess(N, G, A) ->
    C = abs((G*G - N)),
    if
	(C =< A) ->
	    ok;
	true ->
	    no
    end.
				      
