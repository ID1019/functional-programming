-module(test).

-compile(export_all).

start(N) ->
    spawn(fun() -> create(N) end).
		   

create(N) ->
    T1 = now(),
    create(N, fun() -> receive _ -> ok end end),
    T2 = now(), 
    T = timer:now_diff(T2,T1),
    io:format("~w processes created in ~w ms~n", [N, T div 1000]),
    exit(argh).

create(0,_) ->
    ok;
create(N, F) ->
    spawn_link(F),
    create(N-1, F).
