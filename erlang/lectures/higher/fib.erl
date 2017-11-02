-module(fib).

-compile(export_all).


fib() -> fun() -> fib(1,1) end.

fib(F1, F2) -> [F1 | fun() -> fib(F2, F1+F2) end].


fib(N) ->
    seq(N, fib()).

seq(0,_) ->
    ok;
seq(N, F) ->
    [Next|F1] = F(),
    io:format("~w~n", [Next]),
    seq(N-1, F1).


    
