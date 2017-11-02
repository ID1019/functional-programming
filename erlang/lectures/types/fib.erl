-module(fib).

-compile(export_all).

%-spec bench(integer()) -> ok.

bench(N) ->
    T1 = time( fun() -> fib1(N) end),
    T2 = time( fun() -> fib2(N) end),
    T3 = time( fun() -> fib3(N) end),
    T4 = time( fun() -> fib4(N) end),
    lists:foreach(fun({P,V}) -> io:format("~14s: ~4.2f~n", [P, V]) end, 
        [{"no guards", T1/T1},
         {"is_integer", (T2)/T1},
         {"N > 1", (T3)/T1},
         {"both", (T4)/T1}]),
    ok.


time(F) ->
    Start = erlang:system_time(micro_seconds),
    F(),
    erlang:system_time(micro_seconds) - Start.


%-spec fib1(any()) -> integer().

fib1(0) -> 0;
fib1(1) -> 1;
fib1(N) -> 
    fib1(N-1) + fib1(N-2).

%-spec fib2(integer()) -> integer().

fib2(0) -> 0;
fib2(1) -> 1;
fib2(N) when is_integer(N)  -> 
    fib2(N-1) + fib2(N-2).

%-spec fib3(integer()) -> integer().

fib3(0) -> 0;
fib3(1) -> 1;
fib3(N) when N > 1  -> 
    fib3(N-1) + fib3(N-2).

%-spec fib4(integer()) -> integer().

fib4(0) -> 0;
fib4(1) -> 1;
fib4(N) when is_integer(N) and (N > 1) -> 
    fib4(N-1) + fib4(N-2).



     


    
