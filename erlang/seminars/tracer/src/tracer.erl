-module(tracer).

-export([tracer/2]).

-define(Black, {0,0,0}).
-define(White, {1,1,1}).

tracer(Camera, Objects) ->
    {W, H} = camera:size(Camera),
    Xs = lists:seq(1, W),
    Ys = lists:seq(1, H),
    [[trace(X, Y, Camera, Objects) || X <- Xs] || Y <- Ys].

trace(X, Y, Camera, Objects) ->
    Ray = camera:ray(X, Y, Camera),
    trace(Ray, Objects).

trace(Ray, Objects) ->
    case intersect(Ray, Objects) of
        {inf, _} ->
	    ?Black;
	{_, _} -> 
	    ?White
    end.

intersect(Ray, Objs) ->
   lists:foldl(fun(Obj, Sofar) -> 
		       {Dist, _} = Sofar,
		       case objects:intersect(Obj, Ray) of
			   {ok, D} when D < Dist ->
			       {D, Obj};
			   _ ->
			       Sofar
		       end
	       end,
	       {inf, no},
	       Objs).

