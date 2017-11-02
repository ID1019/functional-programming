-module(tracer3).

-export([tracer/2]).

-define(Delta, 0.001).

tracer(Camera, World) ->
    {W, H} = camera:size(Camera),
    Xs = lists:seq(1, W),
    Ys = lists:seq(1, H),
    [[trace(X, Y, Camera, World) || X <- Xs] || Y <- Ys].

trace(X, Y, Camera, World) ->
    Ray = camera:ray(X, Y, Camera),
    Depth = world1:depth(World),
    trace(Ray, Depth, World).

trace(_Ray, 0, World) ->
    world1:background(World);
trace(Ray, Depth, World) ->
    Objs = world1:objects(World),
    case intersect(Ray, Objs) of
        {inf, _} ->
	    world1:background(World);
	{D, Obj} -> 
	    O = objects1:origin(Ray),
	    L = objects1:direction(Ray),

	    I = vector:add(O, vector:smul(L, (D-?Delta))),

	    Normal = objects1:normal(I, Obj),

	    Visible = visible(I, world1:lights(World), Objs),

	    Illumination = lights1:combine(I, Normal,  Visible),

	    R = objects1:ray(I, reflection(L, Normal)),

	    Reflection = trace(R, Depth-1, World),

	    lights1:illuminate(Obj, Reflection, Illumination, World)
    end.

intersect(Ray, Objs) ->
   lists:foldl(fun(Obj, Sofar) -> 
		       {Dist, _} = Sofar,
		       case objects1:intersect(Obj, Ray) of
			   {ok, D} when D < Dist ->
			       {D, Obj};
			   _ ->
			       Sofar
		       end
	       end,
	       {inf, no},
	       Objs).


visible(Point, Lights, Objs) ->
    lists:filter(fun(Light) -> clear(Point, lights1:origin(Light), Objs)  end, Lights).

clear(Point, Origin, Objs) ->
    Dir = vector:normalize(vector:sub(Origin, Point)),
    lists:foldl(fun(Obj, Acc) ->
			case Acc of
			    false ->
				false;
			    true ->
				case objects1:intersect(Obj, {ray, Point, Dir}) of
				    no ->
					true;
				    _ ->
					false
				end
			end
		end,
		true,
		Objs).

reflection(L, N) ->
    vector:sub(L,vector:smul(N,2*vector:dot(L,N))).
