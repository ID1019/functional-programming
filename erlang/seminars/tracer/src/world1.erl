-module(world1).

-compile(export_all).

-define(Background, {0.0,0.0,0.0}).

-define(Depth, 2).

-define(Ambient, {0.3,0.3,0.3}).

-define(Refraction, 1).

-record(world, { objects=[], 
		 lights=[], 
		 background=?Background, 
		 depth=?Depth, 
		 ambient=?Ambient,
		 refraction=?Refraction
	       }).

world(Objects, Lights) ->
    #world{objects=Objects, lights=Lights}.    

world(Objects, Lights, Opt) ->
    Depth = case lists:keyfind(depth, 1, Opt) of
		{depth, D} ->
		    D;
		false ->
		    ?Depth
	    end,
    Background = case lists:keyfind(background, 1, Opt) of
		{background, B} ->
		    B;
		false ->
		    ?Background
	    end,
    Ambient = case lists:keyfind(ambient, 1, Opt) of
		{ambient, A} ->
		    A;
		false ->
		    ?Ambient
	    end,

    Refraction = case lists:keyfind(refraction, 1, Opt) of
		{ambient, R} ->
		    R;
		false ->
		    ?Refraction
	    end,

    #world{objects=Objects, lights=Lights, depth=Depth, background=Background, ambient=Ambient, refraction=Refraction}.


depth(#world{depth=Depth}) ->
    Depth.

background(#world{background=Background}) ->
    Background.

ambient(#world{ambient=Ambient}) ->
    Ambient.

lights(#world{lights=Lights}) ->
    Lights.

objects(#world{objects=Objects}) ->
    Objects.

refraction(#world{refraction=R}) ->
    R.


