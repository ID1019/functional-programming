-module(mem).

-export([new/1, read/2, write/3, list/1]).

new(List) -> 
    Cells = lists:map(fun(E) -> cell:start(E) end, List),
    {mem, erlang:list_to_tuple(Cells)}.

list({mem, M}) ->
    Cells = tuple_to_list(M),
    lists:map(fun(C) -> cell:read(C) end, Cells).
		      

read({mem, M}, N) ->
    %%io:format("element ~w ~n", [N]),
    Cell = erlang:element(N, M),
    cell:read(Cell).

write({mem, M}, N, V) ->
    Cell = erlang:element(N, M),
    cell:write(Cell, V).
