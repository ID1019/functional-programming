-module(qsort).

-compile(export_all).

test() ->
    Array = mem:new([2,0,3,6,5,8,9,7,1,4]),
    qsort(Array, 1, 10),
    mem:list(Array).


qsort(Array, I, J) when J =< I ->
    Array;
qsort(Array, I, J) ->
    %%io:format("qsort ~w, ~w of ~w~n", [I,J, mem:list(Array)]),
    K = partition(Array, I, J),
    %%io:format("partition done K = ~w~n", [K]),
    qsort(Array, I, K-1),
    qsort(Array, K, J).

partition(Array, I, J) ->
    %%io:format("partition ~w ~w~n",[I,J]),
    P = mem:read(Array, J),
    left(Array, P, I, J).
    
left(_, _, I, I) ->
    %%io:format("left done ~w~n",[I]),
    I;
left(Array, P, I, J) ->
    %%io:format("left ~w ~w~n",[I,J]),
    VI = mem:read(Array, I),
    if 
	VI =< P ->
	    left(Array, P, I+1, J);
	true ->
	    right(Array, P, I, J)
    end.

right(_, _, J, J) ->
    %%io:format("right done ~w~n",[J]),
    J;
right(Array, P, I, J) ->
    %%io:format("right ~w ~w~n",[I,J]),
    VJ = mem:read(Array, J),
    if 
	VJ > P ->
	    right(Array, P, I, J-1);
	true ->
	    swap(Array, I, J),
	    left(Array, P, I+1, J)
    end.


swap(Array, I, J) ->
    VI = mem:read(Array, I),    
    VJ = mem:read(Array, J),
    mem:write(Array, I, VJ),
    mem:write(Array, J, VI).    



