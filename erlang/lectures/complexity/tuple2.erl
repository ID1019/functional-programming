-module(tuple2).

-compile(export_all).

lookup(K, Tuple) -> element(K, Tuple).
modify(K, V, Tuple) -> setelement(K, Tuple, {value, V}).
delete(K, Tuple) -> setelement(K, Tuple, false).

insert(K, V, Tuple) -> 
    erlang:insert_element(K, Tuple, {value, V}).


bench() ->
    N = 1000000,
    io:format("# Benchmark of tree opertations, (~w opertions), times in ms~n#~n", [N]),
    io:format("#~7s ~8s ~8s ~n", ["n", "lookup", "modify"]),
  
    Ls = [32,64,128,256,512,1024],

    Bench = fun(L) ->
       %% create a list of L random numbers
       Sample = random(N, L),
       Table = list_to_tuple(lists:seq(1,L)),
       Tl  = time(fun() -> loop(Sample, fun(S) -> lookup(S, Table) end) end),
       Tm  = time(fun() -> loop(Sample, fun(S) -> modify(S, foo, Table) end) end),
       io:format("~8w ~8w ~8w~n", [L, round(Tl/1000), round(Tm/1000)])
       end,
    lists:foreach(Bench, Ls).

random(0,_) ->
    [];
random(N,L) ->
    R = trunc(random:uniform() * L) +1,
    [R | random(N-1, L)].


time(F)->
    %% time in micro seconds
    T1 = now(),
    F(),
    T2 = now(),
    timer:now_diff(T2, T1).


loop([], _) ->
    ok;
loop([S|Sr], Fun) ->
    Fun(S),
    loop(Sr, Fun).


