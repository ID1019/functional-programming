-module(server).

-export([start/7]).

%%% The start/7 process will start a mandel server and a print process

start(Width, Height, X, Y, K, Depth, File) ->
    {ok, spawn(fun() -> init(Width, Height, X, Y, K, Depth, File) end)}.

init(Width, Height, X, Y, K, Depth, File) ->
    {ok, Ctrl} = print:start(File, Width, Height),
    %% Sending lambda expressions works if client side has exactly the
    %% same code base. We try to avoid this when doing it in class.

    %% Trans = fun(W, H) -> {X + K*(W-1), Y-K*(H-1)} end,
    Trans = {trans, X, Y, K},

    rows(Width, Height, Trans, Depth, Ctrl).


rows( _, 0, _, _, _)->
    io:format("done~n", []),
    done();
rows(W, H, Tr, Depth, Ctrl) ->
    receive 
	{request, From} ->
	    io:format("sending request to ~w~n", [From]),
	    From ! {task, W, H, Tr, Depth, Ctrl},
	    Ctrl ! go, 
	    rows(W, H-1, Tr, Depth, Ctrl);
	stop ->
	    ok;
	Strange ->
	    io:format("strange message ~w~n", [Strange]),	    
	    rows(W, H, Tr, Depth, Ctrl)
    end.
    
done() ->	    
    receive 
	{request, From} ->
	    From ! done,
	    done();
	stop ->
	    ok
    end.
    



    
    










		  
