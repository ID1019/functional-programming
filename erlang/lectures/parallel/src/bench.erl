-module(bench).


-compile(export_all).

fib(0) -> 1;
fib(1) -> 1;
fib(N) ->  
    F1 = fib(N-1),
    F2 = fib(N-2),
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








bench(N, F, C) ->
    io:format("threads  min    q25    median   q75   max~n", []),
    Fun = fun() -> fix(F,C) end,
    lists:foreach(fun(Threads) ->
			  {Min, Q25, Med, Q75, Max} = loop(Fun, 100, Threads),
			  io:format("~w  ~w  ~w  ~w  ~w  ~w~n", [Threads, Min, Q25, Med, Q75, Max])
		  end,
		  lists:seq(1, N)),
    ok.
   
    

loop(Fun, Loop, Threads) ->
    erlang:system_flag(schedulers_online, Threads),
    Times = lists:map(fun(_) -> {T,_} = timer:tc(Fun), T end, lists:seq(1,Loop)),
    Sorted = lists:sort(Times),
    L = length(Sorted),
    Min = hd(Sorted),
    Q25 = lists:nth(trunc(25*(L / 100)), Sorted),
    Med = lists:nth(L div 2, Sorted),
    Q75 = lists:nth(trunc(75*(L / 100)), Sorted),
    Max = lists:last(Sorted),
    {Min, Q25, Med, Q75, Max}.






    


    
