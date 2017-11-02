
-module(enigma).

-export([encode/1]).

encode(Text) ->
    R1 = rotor(a),
    encode(Text, R1).

encode([], _) ->
    [];
encode([Char|Rest], Rotor) ->
    [transform(Char, Rotor)| encode(Rest, rotate(Rotor))].

transform(Char, {N, Map}) ->
    element(Char+N, Map).

rotate(x1{N, Map}) ->
    {((N+1) rem 6), Map}.
    
rotor(a) -> 
    {0, {2,1,3,4,0,5}}.

