%% This is not a module, its code that you can add to the existing modules. 

%% If we want a easy way to extend the properties of spheres. 

-define(Color, {1,1,1}).
-define(Brilliance, {0.8,0.8,0.8}).

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


