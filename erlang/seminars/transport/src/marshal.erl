-module(marshal).

-compile(export_all).

-define(Atom, 1).
-define(Tuple, 2).

encode(Str) when is_atom(Str) ->
    Chars = atom_to_list(Str),
    L = length(Chars),
    [?Atom, L | Chars];
encode(Str) when is_tuple(Str) ->
    S = tuple_size(Str),
    Elements = encode_elements(S, Str),
    [?Tuple, S | Elements].

encode_elements(0, _) ->
    [];
encode_elements(I, Tuple) ->
    encode(element(I, Tuple)) ++ encode_elements(I-1, Tuple).



decode([?Atom, L | Encoded]) ->
    {Chars, Rest} = lists:split(L, Encoded),
    Atom = list_to_atom(Chars),
    {Atom, Rest};
decode([?Tuple, I | Encoded]) ->
    Elements = decode_elements(I, Encoded, []),
    Tuple = list_to_tuple(Elements),
    {Tuple, []}.

decode_elements(0, [], Elements) ->	
    Elements;
decode_elements(I, Encoded, Elements) ->	
    {Element, Rest} = decode(Encoded),
    decode_elements(I-1, Rest, [Element|Elements]).





    

