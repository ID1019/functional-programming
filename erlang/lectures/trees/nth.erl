-module(nth).

-compile(export_all).

dummy(_,_) ->  ok.

nth_r(1, [H|_]) -> H;
nth_r(N, [_|T]) -> nth_r(N-1, T).
    

nth_l( 1, [N|_]) -> N;
nth_l( 2, [_,N|_]) -> N;
nth_l( 3, [_,_,N|_]) -> N;
nth_l( 4, [_,_,_,N|_]) -> N;
nth_l( 5, [_,_,_,_,N|_]) -> N;
nth_l( 6, [_,_,_,_,_,N|_]) -> N;
nth_l( 7, [_,_,_,_,_,_,N|_]) -> N;
nth_l( 8, [_,_,_,_,_,_,_,N|_]) -> N;
nth_l( 9, [_,_,_,_,_,_,_,_,N|_]) -> N;
nth_l(10, [_,_,_,_,_,_,_,_,_,N]) -> N.

nth_e(N, T) ->
    element(N, T).

nth_t( 1, {N,_,_,_,_,_,_,_,_,_}) -> N;
nth_t( 2, {_,N,_,_,_,_,_,_,_,_}) -> N;
nth_t( 3, {_,_,N,_,_,_,_,_,_,_}) -> N;
nth_t( 4, {_,_,_,N,_,_,_,_,_,_}) -> N;
nth_t( 5, {_,_,_,_,N,_,_,_,_,_}) -> N;
nth_t( 6, {_,_,_,_,_,N,_,_,_,_}) -> N;
nth_t( 7, {_,_,_,_,_,_,N,_,_,_}) -> N;
nth_t( 8, {_,_,_,_,_,_,_,N,_,_}) -> N;
nth_t( 9, {_,_,_,_,_,_,_,_,N,_}) -> N;
nth_t(10, {_,_,_,_,_,_,_,_,_,N}) -> N.

bench() ->
    N = 100000,
    Ls = [1,2,3,4,5,6,7,8,9,10],
    io:format("Benchmark for finding n'th element, ~w times, in a list or tuple, all times in ms.~n~n", [N]),
    io:format("~8s ~8s ~8s ~8s ~8s ~8s~n", ["n", "dummy", "recursive", "explicit", "element", "tuple"]),
    Bench = fun(L) ->
       List = lists:seq(1,10),
       Tuple = list_to_tuple(List),
       Td = time(N, fun() -> dummy(L,List) end),
       Tr = time(N, fun() -> nth_r(L,List) end),
       Tl = time(N, fun() -> nth_l(L,List) end),
       Te = time(N, fun() -> nth_e(L,Tuple) end),
       Tt = time(N, fun() -> nth_t(L,Tuple) end),
       io:format("~8w ~8.2f ~8.2f ~8.2f ~8.2f ~8.2f~n", [L, Td/1000, Tr/1000, Tl/1000, Te/1000, Tt/1000])
       end,
    lists:foreach(Bench, Ls).

time(N, F)->
    %% time in micro seconds
    T1 = now(),
    loop(N, F),
    T2 = now(),
    timer:now_diff(T2, T1).

loop(N, Fun) ->
  if N == 0 -> ok; true -> Fun(), loop(N-1, Fun) end.
