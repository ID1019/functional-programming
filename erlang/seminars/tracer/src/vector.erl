-module(vector).

%%% A small module for 3D vector operations.

-export([smul/2, sub/2, add/2, dot/2, scale/2, normalize/1, norm/1, sq/1, cross/2]).

%% Scalar multiplication 

smul({X1,X2,X3}, S) ->
    {X1*S, X2*S, X3*S}.

%% Addition and subtarction

sub({X1,X2,X3},{Y1,Y2,Y3}) ->
    {X1-Y1,X2-Y2,X3-Y3}.

add({X1,X2,X3},{Y1,Y2,Y3}) ->
    {X1+Y1,X2+Y2,X3+Y3}.

%% Dot product

dot({X1,X2,X3},{Y1,Y2,Y3}) ->
    X1*Y1 + X2*Y2 + X3*Y3.


%% Scaling a vector to a specified lenth

scale(X, L) ->
    N = norm(X),
    %% this will crash if N == 0!
    smul(X, L/N).

%% Normalize by scaling to norm 1.

normalize(X) ->
    scale(X,1).

%% Square of a vector

sq(X) ->
    dot(X,X).

%% The norm (length) of a vector

norm({X1,X2,X3}) ->
    math:sqrt(X1*X1+X2*X2+X3*X3).

%% Cross product, used to find a vector that as ortogonal to both X
%% and Y

cross({X1,X2,X3},{Y1, Y2, Y3}) ->
    {X2*Y3-X3*Y2, X3*Y1-X1*Y3,  X1*Y2-X2*Y1}.


 
