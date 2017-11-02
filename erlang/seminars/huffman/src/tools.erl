-module(tools).

-compile(export_all).

%% building an encode tree using a list of codes on the form {".-..-", Char}

encode_tree(Codes) ->
    Sorted = lists:sort(Codes),
    N = length(lists:sort(Codes)),
    encode_tree(N, Sorted).

encode_tree(1, [{Ac, A}]) ->
    {node, Ac, A, nil, nil};
encode_tree(2, [{Ac, A}, {Bc, B}]) ->
    {node, Ac, A, nil, {node, Bc, B, nil, nil}};
encode_tree(3, [{Ac, A}, {Bc, B}, {Cc, C}]) ->
    {node, Bc, B, {node, Ac, A, nil, nil}, {node, Cc, C, nil, nil}};
encode_tree(N, Codes) ->
    L = N div 2,
    {Low, [{Ac, A}|High]} = lists:split(L, Codes),
    {node, Ac, A, encode_tree(L, Low), encode_tree(N-L-1, High)}.


%% building a decode tree using a list of codes on the form {Char, ".-..-"}

decode_tree(Codes) ->
    decode_tree(Codes, nil).

decode_tree([], Tree) ->
    Tree;
decode_tree([{Char, Code}|Rest], Tree) ->
    decode_tree(Rest, decode_tree(Code, Char, Tree)).

decode_tree([], Char, {node, na, Left, Right}) ->
    {node, Char, Left, Right};
decode_tree([], Char, nil) ->
    {node, Char, nil, nil};
decode_tree([$-|Rest], Char, {node, C, Left, Right}) ->
    {node, C,  decode_tree(Rest, Char, Left), Right};
decode_tree([$.|Rest], Char, {node, C, Left, Right}) ->
    {node, C,  Left, decode_tree(Rest, Char, Right)};
decode_tree([$-|Rest], Char, nil) ->
    {node, na,  decode_tree(Rest, Char, nil), nil};
decode_tree([$.|Rest], Char, nil) ->
    {node, na,  nil, decode_tree(Rest, Char, nil)}.


%% reading the codes from a file each line on the form [_,... 32,Char,10]

codes(File) ->
    {ok, Fd} = file:open(File, [read]),
    Lines = read(Fd),
    file:close(Fd),    
    lists:map(fun(L) -> trans(L) end, Lines).

trans(Line) ->
    trans(Line, []).

trans([32, Char, 10], Code) ->
    {Char, lists:reverse(Code)};
trans([32|Rest], Code) ->
    trans(Rest, Code);    
trans([I|Rest], Code) ->
    trans(Rest, [I|Code]).

read(Fd) ->
    Line = file:read_line(Fd),
    read(Line, Fd).

read(eof, _Fd) ->
    [];
read({ok, Line}, Fd) ->
    [Line|read(Fd)].


