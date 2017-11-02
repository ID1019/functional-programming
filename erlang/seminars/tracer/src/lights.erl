%%% This module will handle everythning that is related to lights and
%%% color composition. Colors are represented as {R,G,B} where the
%%% values are 0.0 to 1.0. 

-module(lights).

-export([light/2, origin/1, illuminate/3]).

-record(light, {origin, color={1.0,1.0,1.0}}).

light(Origin, Color) ->
    #light{origin=Origin, color=Color}.

origin(#light{origin=Origin}) ->
    Origin.

%%% find the color of a point given color of object, combined light
%%% and ambient light

illuminate(Obj, Ill, World) ->
    Color = objects:color(Obj),
    Ambient = world:ambient(World),
    ill(Color, mul(Ill, Ambient)).
    
%%% combine two light sources

mul({R1,G1,B1}, {R2,G2,B2}) ->
    {(1 - ((1-R1)*(1-R2))), (1 - ((1-G1)*(1-G2))), (1 - ((1-B1)*(1-B2)))}.


%%% illuminate the surface with a colored light

ill({R1,G1,B1}, {R2,G2,B2}) ->
    {R1*R2, G1*G2, B1*B2}.





