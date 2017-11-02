-module(test).

-compile(export_all).

server(Img, Size, Depth, Name) ->
    File = atom_to_list(Name) ++ ".ppm",
    {X, Y, X1} = image(Img),
    {Width, Height, K} = size(Size, X, X1),
    {ok, Server} = server:start(Width, Height, X, Y, K, Depth, File),
    register(server, Server).


sky(Img, Size, Depth) ->
    bench(1, Img, Size, Depth),
    bench(2, Img, Size, Depth),    
    bench(4, Img, Size, Depth),
    bench(6, Img, Size, Depth),
    bench(8, Img, Size, Depth),
    bench(12, Img, Size, Depth),
    bench(16, Img, Size, Depth).


pro(Img, Size, Depth) ->
    bench(1, Img, Size, Depth),
    bench(2, Img, Size, Depth),    
    bench(3, Img, Size, Depth),    
    bench(4, Img, Size, Depth).

bench(N, Img, Size, Depth)->
    erlang:system_flag(schedulers_online, N),
    io:format("~w cores -- ", [N]),
    S = erlang:system_time(milli_seconds),
    {X, Y, X1} = image(Img),
    {Width, Height, K} = size(Size, X, X1),
    mandelp:mandelbrot(Width, Height, X, Y, K, Depth),
    T = erlang:system_time(milli_seconds) - S,
    io:format("~w ms~n", [T]).

%%% Given the upper left corner, the size information, the Depth and a
%%% Name this procedure will calculate an image and print it to a .ppm
%%% file.

print({X, Y}, {Width, Height, K}, Depth, Name) ->
    File = atom_to_list(Name) ++ ".ppm",
    T0 = erlang:system_time(milli_seconds),
    Image = mandelp:mandelbrot(Width, Height, X, Y, K, Depth),
    ppm:write(File, Image),
    T = erlang:system_time(milli_seconds) - T0,
    io:format("picture generated and printed in ~w ms~n", [T]).

%%% size(Size, X, X1) -> {Width, Height, K} where Size is small, large
%%% or huge, and X, X1 are teh left- and rightermost values. The
%%% function will return a Width and Height value taht has the 16:9
%%% ratio. The K value is the step factor for each pixle. 

size(small, X, X1) ->
    Width = 960,
    Height = 540,
    K = (X1 - X)/Width,
    {Width, Height, K};
size(large, X,X1) ->
    Width = 1920,
    Height = 1080,
    K = (X1 - X)/Width,
    {Width, Height, K};
size(long, X,X1) ->
    Width = 2560,
    Height = 1080,
    K = (X1 - X)/Width,
    {Width, Height, K};    
size(huge, X, X1) ->
    Width = 3840, 
    Height = 2160, 
    K = (X1 - X)/Width,
    {Width, Height, K}.


%%% image(Name) -> {X,Y,X1} where X,Y is the upper left corner and X1
%%% the rightermost position.

image(mandel) ->
    {-2.6,1.2,1.6};
image(waves) ->
    {-0.14,0.85,-0.13};
image(forest) ->
    {-0.136,0.85,-0.134}.


    




    
	

    
