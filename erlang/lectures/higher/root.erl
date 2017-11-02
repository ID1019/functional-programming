-module(root).

-compile(export_all).

-define(Guess, 12).

-define(Acc, 0.01).

all(N) ->
    all(N,1).

all(N, A) ->
    {sq(N, ?Guess, A), fun() -> all(N, A/10) end}.
			       
%% How do we rewrite this to compute the root inccrementaly instead of
%% starting from the beginning?
  


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
				      
