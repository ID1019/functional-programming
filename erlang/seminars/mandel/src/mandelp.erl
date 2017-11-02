-module(mandelp).

-export([mandelbrot/6]).

mandelbrot(Width, Height, X, Y, K, Depth) ->
    Trans = fun(W, H) -> {X + K*(W-1), Y-K*(H-1)} end,
    rows(Width, Height, Trans, Depth, self()),
    collect(Height, []).


collect(0, Rows) ->
    Rows;
collect(H, Rows) ->
    receive 
	{row, H, Row} ->
	    collect(H-1, [Row|Rows])
    end.


rows( _, 0, _, _, _)->
    ok;
rows(W, H, Tr, Depth, Ctrl) ->
    spawn(fun() -> report(W, H, Tr, Depth, Ctrl) end),
    rows(W, H-1, Tr, Depth, Ctrl).

report(W, H, Tr, Depth, Ctrl) ->
    Row = row(W, H, Tr, Depth, []),
    Ctrl ! {row, H, Row}.

row(0, _, _, _, Row) ->
    Row;
row(W, H, Tr, Depth, Row) ->
    {X,Y} = Tr(W,H),
    C = cmplx:new(X,Y),
    Res =  brot:mandelbrot(C, Depth),
    Color = color:convert(Res, Depth),
    row(W-1, H, Tr, Depth, [Color|Row]).


    



