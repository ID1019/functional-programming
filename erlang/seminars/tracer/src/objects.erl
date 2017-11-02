-module(objects).

-record(ray, {origin={0,0,0}, direction={1,1,1}}).

-record(sphere, {radius=2, center}).

-export([ray/2, sphere/2,  intersect/2,  normal/2, origin/1, direction/1]).

ray(Origin, Direction) ->
    #ray{origin=Origin, direction=Direction}.

origin(#ray{origin=O}) -> O.

direction(#ray{direction=L}) -> L.     

sphere(Radius, Center) ->
    #sphere{radius=Radius, center=Center}.


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



