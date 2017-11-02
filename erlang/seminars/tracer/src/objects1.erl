-module(objects1).

-define(Color, {1.0,0.4,0.4}).

-define(Brilliance, 0). % 

-define(Transparency, 0). % 

-define(Refraction, 1.5). % glass 

-record(ray, {origin={0,0,0}, direction={1,1,1}}).

-record(sphere, {radius=2, center, color=?Color, brilliance=?Brilliance, transparency=?Transparency, refraction=?Refraction}).

-export([ray/2, sphere/2, sphere/3, intersect/2, color/1, normal/2, brilliance/1, transparency/1, refraction/1, origin/1, direction/1, vector/2]).

ray(Origin, Direction) ->
    #ray{origin=Origin, direction=Direction}.

origin(#ray{origin=O}) -> O.

direction(#ray{direction=L}) -> L.     

%%%% creat a vector in the direction of the ray with a given length

vector(#ray{origin=Origin, direction=Direction}, Length) ->
    vector:add(Origin, vector:smul(Direction, Length)).


sphere(Radius, Center) ->
    #sphere{radius=Radius, center=Center}.

sphere(Radius, Center, Opt) ->
    Color = case lists:keyfind(color, 1, Opt) of
                {color, C} ->
                    C;
                false ->
                    ?Color
            end,
    Brilliance = case lists:keyfind(brilliance, 1, Opt) of
		     {brilliance, B} ->
			 B;
		     false ->
			 ?Brilliance
		 end,
    Transparency = case lists:keyfind(transparency, 1, Opt) of
		       {transparency, T} ->
			   T;
		       false ->
			   ?Transparency
		   end,
    Refraction = case lists:keyfind(refraction, 1, Opt) of
		       {refraction, R} ->
			   R;
		       false ->
			   ?Refraction
		   end,
    #sphere{radius=Radius, center=Center, color=Color, brilliance=Brilliance, transparency=Transparency, refraction=Refraction}.

color(#sphere{color=Color}) ->  Color.

brilliance(#sphere{brilliance=Brilliance}) ->
    Brilliance.

transparency(#sphere{transparency=Transparency}) ->
    Transparency.

refraction(#sphere{refraction=Refraction}) ->
    Refraction.

normal(I, #sphere{center=C}) ->
    vector:normalize(vector:sub(I,C)).  

intersect(#sphere{radius=R, center=C}, #ray{origin=O, direction=L}) ->
    K = vector:sub(C,O),
    A = vector:dot(L, K),
    A2 = math:pow(A, 2),
    K2 = math:pow(vector:norm(K),2),
    R2 = math:pow(R,2),
    T2 =  A2 - K2 + R2,
    closest(T2, A).

closest(T2, A) ->
    if 	T2 < 0 ->   
	    no;
	true ->
	    T = math:sqrt(T2),
	    D1 = A - T,  D2 = A + T,
	    if 
		(D1 > 0.0) and (D2 > 0.0) -> 
		    {ok, min(D1,D2)};
		(D1 > 0.0)  > 
		    {ok, D1};
		(D2 > 0.0) -> 
		    {ok, D2} ;
		true ->
		    no
	    end
    end.



