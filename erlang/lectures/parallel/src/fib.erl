-module(fib).

-compile(export_all).



fib(0) -> 1;
fib(1) -> 1;
fib(N) ->  
    F1 = fib(N-1),
    F2 = fib(N-2),
    F1 + F2.








fip(0) -> 1;
fip(1) -> 1;
fip(N)  -> 
    Ref = make_ref(),
    parallel(fun() -> fip(N-1) end, Ref),
    parallel(fun() -> fip(N-2) end, Ref),
    F1 = collect(Ref),
    F2 = collect(Ref),
    F1 + F2.









fix(0,_) -> 1;
fix(1,_) -> 1;
fix(N,M) when N > M->
    Ref = make_ref(),
    parallel(fun() -> fix(N-1, M) end, Ref),
    parallel(fun() -> fix(N-2, M) end, Ref),
    F1 = collect(Ref),
    F2 = collect(Ref),
    F1 + F2;
fix(N,_)  ->  fib(N).

parallel(F, Ref) ->
    Self = self(),
    spawn(fun() -> Res = F(), Self ! {ok, Ref, Res} end).

collect(Ref) ->
    receive
	{ok, Ref, Res} ->
	    Res
    end.







naive() ->	
    {Fib, _} = timer:tc(fun() -> fib(30) end),
    {Fip, _} = timer:tc(fun() -> fip(30) end),
    io:format("fib(30)  sequential: ~w ms, naive parallel ~w ms~n", [Fib div 1000, Fip div 1000]).


better() ->	
    {Fib, _} = timer:tc(fun() -> fib(40) end),
    io:format("fib(40)  sequential: ~w ms~n", [Fib div 1000]),
    bench(40,38),
    bench(40,36),
    bench(40,34),
    bench(40,32),
    bench(40,30),
    bench(40,28),
    bench(40,26),
    bench(40,24),
    bench(40,22),
    bench(40,20).

bench(N, M) ->
    {T, _} = timer:tc(fun() -> fix(N, M) end),
    io:format("fib(~w,~w) : ~w ms~n", [N, M, T div 1000]).

    

    
    
		     
			
		  
     
    

