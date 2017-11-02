-module(mandeln).

-export([mandelbrot/6]).


mandelbrot(Width, Height, X, Y, K, Depth) ->
    Trans = fun(W, H) -> 
		    cmplx:new(X + K*(W-1), Y-K*(H-1))
	    end,
    rows(Width, Height, Trans, Depth, []).

rows( _, 0, _, _, Rows)->
    Rows;
rows(W, H, Tr, Depth, Rows) ->
    Row = row(W, H, Tr, Depth, []),
    rows(W, H-1, Tr, Depth, [Row|Rows]).

row(0, _, _, _, Row) ->
    Row;
row(W, H, Tr, Depth, Row) ->
    C = Tr(W,H),
    Res =  native:mandelbrot(C, Depth),
    Color = color:convert(Res, Depth),
    row(W-1, H, Tr, Depth, [Color|Row]).


    



