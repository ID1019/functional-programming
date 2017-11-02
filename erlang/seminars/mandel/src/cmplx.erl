-module(cmplx).

-export([new/2,add/2, sqr/1, abs/1, mandelbrot/2, mandelbrot_nif/2]).

new(X, Y) ->
    {X, Y}.

add({X1,Y1}, {X2,Y2}) ->
    {X1+X2, Y1+Y2}.

sqr({X,Y}) ->
    {X*X - Y*Y, 2*X*Y}.

abs({X,Y}) ->
    math:sqrt(X*X+Y*Y).

%% This is doing the mandelbrot calculation using the NIF interface. 

mandelbrot_nif({Cr, Ci}, M) ->
    depth:test(Cr, Ci, M).


mandelbrot({Cr, Ci}, M) ->
     Zr = 0,
     Zi = 0,
     test(0, Zr, Zi, Cr, Ci, M).

%% Let's do the depth calculation as quickly as possible in Erlang. We
%% will avoid consytructing tuples and do as few operations as possible. 

test(M, _Zr, _Zi, _Cr, _Ci, M) ->
    0;
test(I, Zr, Zi, Cr, Ci, M) ->
    Zr2 = Zr*Zr,
    Zi2 = Zi*Zi,
    A2 = Zr2 + Zi2,
    if 
	A2 < 4.0 ->
	    Sr = (Zr2 - Zi2) + Cr,
	    Si = 2*Zr*Zi + Ci,
	    test(I+1, Sr, Si, Cr, Ci, M);
	true ->
	    I
    end.

