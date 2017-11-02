-module(tracer4).

-export([tracer/2, reflection/2, refraction/4]).

-define(Delta, 0.001).

tracer(Camera, World) ->
    {W, H} = camera:size(Camera),
    Xs = lists:seq(1, W),
    Ys = lists:seq(1, H),
    [[trace(X, Y, Camera, World) || X <- Xs] || Y <- Ys].

trace(X, Y, Camera, World) ->
    Ray = camera:ray(X, Y, Camera),
    Depth = world:depth(World),
    Ref = world:refraction(World),
    trace_ray(Ray, 1, Depth, Ref, World).

trace_ray(_Ray, _, 0, _, World) ->
    world:background(World);
trace_ray(_Ray, Cntr, _, _, World) when Cntr < 0.02 ->
    world:background(World);
trace_ray(Ray, Cntr, Depth, X1, World) ->
    Objs = world:objects(World),
    case intersect(Ray, Objs) of
        {inf, _} ->
	    world:background(World);
	{D, Obj} -> 
	    O = objects:origin(Ray),
	    L = objects:direction(Ray),

	    I1 = vector:add(O, vector:smul(L, (D-?Delta))),

	    Normal = objects:normal(I1, Obj),

	    Visible = visible(I1, world:lights(World), Objs),

	    Illumination = lights:combine(I1, Normal,  Visible),

	    R1 = objects:ray(I1, reflection(L, Normal)),

	    Reflection = trace_ray(R1, Cntr * objects:brilliance(Obj), Depth-1, X1, World),

	    X2 = objects:refraction(Obj),

	    case refraction(L, X1, X2, Normal) of
		na -> 
		    %%io:format(".", []),
		    lights:illuminate(Obj, Reflection, Illumination, World);
		Refr ->
		    I2 = vector:add(O, vector:smul(L, (D+?Delta))),
		    R2 = objects:ray(I2, Refr),
		    Refraction = trace_ray(R2, Cntr * objects:transparency(Obj), Depth-1, X2, World),
		    %%io:format("~w  ~w   ~w~n", [Refraction, Reflection, Illumination]),
		    lights:illuminate(Obj, Reflection, Refraction, Illumination, World)
	    end
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


visible(Point, Lights, Objs) ->
    lists:filter(fun(Light) -> clear(Point, lights:origin(Light), Objs)  end, Lights).

clear(Point, Origin, Objs) ->
    Dir = vector:normalize(vector:sub(Origin, Point)),
    lists:foldl(fun(Obj, Acc) ->
			case Acc of
			    false ->
				false;
			    true ->
				case objects:intersect(Obj, {ray, Point, Dir}) of
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


refraction(L, I, J, N) ->
    R = I/J,   %% I and I are the refractive indexes 
    C = - vector:dot(N,L),  
    Q = (1 - (math:pow(R,2) * (1- math:pow(C,2)))),
    if
	Q < 0 -> na;  %% we're moving from a thicker media to a thinner and the angle is too large
	true -> vector:add(vector:smul(L, R), vector:smul(N, ((R*C) - math:sqrt(Q))))
    end.


