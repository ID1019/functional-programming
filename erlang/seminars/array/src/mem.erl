-module(mem).

-compile(export_all).

new(Values) ->
    Pids = lists:map(fun(V) -> cell:start(V) end, Values),
    list_to_tuple(Pids).

set(Array, N, V) ->
    cell:set(element(N, Array), V).

get(Array, N) ->
    cell:get(element(N, Array)).

get_asyn(Array, N) ->
    cell:get_asyn(element(N, Array)).

get_answ(Ref) ->
    cell:get_answ(Ref).

sum(Array) ->    
    Ids = lists:seq(1, size(Array)),
    Refs = lists:map(fun(I) -> get_asyn(Array, I) end, Ids),
    lists:foldl(fun(R, A) -> {ok, V} = get_answ(R), A+V end, 0, Refs).

			 


