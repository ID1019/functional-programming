-module(color).

-export([convert/2, blue/2, red/2]).


%% Convert a scalar, from 0 to max, to a suitabe color represented as
%% {R,G,B} where each element is 0..255. This is just one way of doing
%% it, there are more advanced ways of doing this so do experiment.


convert(D, Max) ->
    test(D, Max).

red(D, Max) ->
    F = D/Max,
    A = F*4,              %% A is [0 - 4.0]
    X = trunc(A),         %% X is [0,1,2,3,4]
    Y = trunc(255*(A-X)), %% Y is [0 - 255]
    case X of
	0 ->
	    {Y, 0, 0};       %% black -> red
	1 ->
	    {255, Y, 0};     %% red -> yellow
	2 ->
	    {255-Y,255, 0};  %% yellow -> green
	3 ->
	    {0, 255, Y};     %% green -> cyan
	4 ->
	    {0,255-Y,255}    %% cyan -> blue
    end.


blue(D, Max) ->
    F = D/Max,
    A = F*4,              %% A is [0 - 4.0]
    X = trunc(A),         %% X is [0,1,2,3,4]
    Y = trunc(255*(A-X)), %% Y is [0 - 255]
    case X of
	0 ->
	    {0, 0, Y};       %% black -> blue
	1 ->
	    {0, Y, 255};     %% blue -> cyan
	2 ->
	    {0, 255, 255-Y};  %% cyan -> green
	3 ->
	    {Y, 255, 0};     %% green -> yellow
	4 ->
	    {255,255-Y,0}    %% yellow-> red
    end.

test(D, Max) ->
    N1 = trunc(Max/32),
    N2 = trunc(Max/16),
    N3 = trunc(Max/8),
    N4 = trunc(Max/4), 
    N5 = trunc(Max/2),

    if 
	D < N1 -> 
	    Y  = trunc(255*(D/N1)),
	    {0, 0, Y};       %%
	D < N2 ->
	    Y  = trunc(255*((D-N1)/(N2-N1))),
	    {0, Y, 255};       %% 
	D < N3 ->
	    Y  = trunc(255*((D-N2)/(N3-N2))),
	    {0, 255, (255-Y)};       %% 
	D < N4 ->
	    Y  = trunc(255*((D-N3)/(N4-N3))),
	    {Y, 255, 0};       %% 
	D < N5 ->
	    Y  = trunc(255*((D-N4)/(N5-N4))),
	    {255, (255 - Y), 0};       %% 
	true ->
	    Y  = trunc(255*((D-N5)/(Max-N5))),
	    {(255 - Y), 0, 0} %%
    end.

