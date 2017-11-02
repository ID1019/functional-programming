-module(cell).

-compile(export_all).

start(N) ->
    spawn(fun() ->  cell(N) end).

set(Pid, V) ->
    Pid ! {set, V},
    ok.

get(Pid) ->
    Pid ! {get, self()},
    receive 
	{ok, V} ->
	    {ok, V}
    end.

get_asyn(Pid) ->
    Ref = make_ref(),
    Pid ! {get, Ref, self()},
    Ref.

get_answ(Ref) ->
    receive 
	{ok, Ref, V} ->
	    {ok, V}
    end.


cell(N) ->
    receive 
	{set, V} ->
	    cell(V);
	{get, Pid} ->
	    Pid ! {ok, N},
	    cell(N);
	{get, Ref, Pid} ->
	    Pid ! {ok, Ref, N},
	    cell(N)
    end.



