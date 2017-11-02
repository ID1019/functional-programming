%%% This module will handle everythning that is related to lights and
%%% color composition. Colors are represented as {R,G,B} where the
%%% values are 0.0 to 1.0. 

-module(lights1).

-compile(export_all).
%-export([light/2, origin/1, illuminate/3, illuminate/4,  illuminate/5, combine/3]).

-record(light, {origin, color={1.0,1.0,1.0}}).


light(Origin, Color) ->
    #light{origin=Origin, color=Color}.

origin(#light{origin=Origin}) ->
    Origin.


%%% find the color of a point given color of object, combined light
%%% and ambient light

illuminate(Obj, Ill, World) ->
    Color = objects1:color(Obj),
    Ambient = world1:ambient(World),
    ill(Color, mul(Ill, Ambient)).
    
%%& also handle lights from reflection

illuminate(Obj, Refl, Ill, World) ->
    Color = objects1:color(Obj),
    Bril = objects1:brilliance(Obj),
    Ambient = world1:ambient(World),
    Surface = ill(Color, mul(Ill, Ambient)),
    mul(Surface, mod(Refl, Bril)).

illuminate(Obj, Refl, Refr, Ill, World) ->
    Color = objects1:color(Obj),
    Bril = objects1:brilliance(Obj),
    Transp = objects1:transparency(Obj),
    Ambient = world1:ambient(World),
    Surface = ill(Color, mul(Ill, Ambient)),
    mul(add(Surface, Refr, Transp), mod(Refl, Bril)).


%%% the combined contribution from all visible light sources

combine(Point, Normal, Lights) ->
    lists:foldl(fun(#light{origin=Src, color=Clr}, Contr) -> 
			mul(contribute(Point, Normal, Src, Clr), Contr)
		end,  
		{0,0,0},
		Lights).


%%% the contribution of a light source give the normal vector

contribute(Point, Normal, Source, {R,G,B}) ->
    Direction = vector:normalize(vector:sub(Source, Point)),
    Cos = (vector:dot(Direction, Normal)),
    {R*Cos, G*Cos, B*Cos}.

%%% combine two light sources

mul({R1,G1,B1}, {R2,G2,B2}) ->
    {(1 - ((1-R1)*(1-R2))), (1 - ((1-G1)*(1-G2))), (1 - ((1-B1)*(1-B2)))}.

%%% add two light sources given a ratio

add({R1,G1,B1}, {R2,G2,B2}, T) ->
    S = 1 -T,
    {R1*S+R2*T,G1*S+G2*T,B1*S+B2*T}.

%%% modifiy a source given a ratio

mod({R1,G1,B1}, T) ->
    {R1*T,G1*T,B1*T}.


%%% illuminate the surface with a colored light

ill({R1,G1,B1}, {R2,G2,B2}) ->
    {R1*R2, G1*G2, B1*B2}.





