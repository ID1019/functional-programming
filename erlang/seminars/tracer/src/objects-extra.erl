%%% This is not a module, its code that you can add to the existing objects module. 


%%% Default color and brilliance.

-define(Color, {1,1,1}).
-define(Brilliance, {0.8,0.8,0.8}).

%%% If we want a easy way to extend the properties of spheres. 

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
    #sphere{radius=Radius, center=Center, color=Color, brilliance=Brilliance}.


%%% Functions to access properties

color(#sphere{color=Color}) ->
    Color.

brilliance(#sphere{brilliance=Brilliance}) ->
    Brilliance.

%%% We need to know the normal vector to a point on the sphere.

normal(I, #sphere{center=C}) ->
    vector:normalize(vector:sub(I,C)).  
