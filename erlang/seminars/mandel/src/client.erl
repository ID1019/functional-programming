-module(client).

-export([start/1]).

start(Server) ->
    spawn(fun() ->  init(Server) end).

init(Server) ->
    client(Server, 0).

client(Server, N) ->
    Server ! {request, self()},
    receive
	{task, W, H, {trans, X, Y, K}, Depth, Ctrl} ->
	    io:format("~w ~w ~n", [W, H]),
            
	    Tr = fun(A, B) -> {X + K*(A-1), Y-K*(B-1)} end,

	    Row = row(W, H, Tr, Depth, []),
	    Ctrl ! {row, H, Row},
	    client(Server, N+1);
	done ->
	    io:format("client completed ~w rounds~n", [N])
    end.

row(0, _, _, _, Row) ->
    Row;
row(W, H, Tr, Depth, Row) ->
    {X,Y} = Tr(W,H),
    C = cmplx:new(X,Y),
    Res =  brot:mandelbrot(C, Depth),
    Color = color:convert(Res, Depth),
    row(W-1, H, Tr, Depth, [Color|Row]).
