-module(karin).

-compile(export_all).

tree(Sample) -> 
    Freq = freq(Sample),
    huffman(Freq).

freq(Sample) -> 
    Base = base(),
    freq(Sample, Base).

freq([], Freq) -> 
      Freq;
freq([Char|Rest], Freq) ->
      freq(Rest, update(Char, Freq)).

update(Char, []) ->
    [{Char, 1}];
update(Char, [{Char, N}|Freq]) ->
    [{Char, N+1}|Freq];
update(Char, [Elem|Freq]) ->
    [Elem|update(Char, Freq)].

huffman(Freq) ->
    Sorted = lists:sort(fun({_,X}, {_,Y}) -> X < Y end, Freq),
    huffman_tree(Sorted).

huffman_tree([{Tree, _}]) ->
    Tree;
huffman_tree([{A,Af}, {B,Bf} | Rest]) ->
    huffman_tree(insert({{A,B}, Af+Bf}, Rest)).

insert(A, []) ->
    [A];
insert({_,Af}=A, [{_,Bf}=B|Rest]) when Af < Bf ->
    [A, B | Rest];
insert(A, [B|Rest]) ->
    [B | insert(A, Rest)].

%%% Generating the table 

table(Tree) ->  
    Codes = codes(Tree, []),
    Sorted = lists:sort(fun({C1, _},{C2, _}) -> C1 < C2 end, Codes),
    Expanded = expand(0, Sorted),
    list_to_tuple(Expanded).

expand(256, _) ->
    [];
expand(N, [{N,Code}|Rest]) ->
    [Code|expand(N+1, Rest)];
expand(N, Codes) ->
    [na|expand(N+1, Codes)].

code_lookup(Char, Table) ->
    {ok, element(Char+1, Table)}.

codes({A,B}, Sofar) ->
    As = codes(A, [0|Sofar]),
    Bs = codes(B, [1|Sofar]),    
    As ++ Bs;
codes(A, Code) ->
    [{A, lists:reverse(Code)}].


%%& encoding the text using the table

encode([], _Table) -> 
    [];
encode([C|Rest], Table) -> 
    case code_lookup(C, Table) of
         {ok, Code} ->
	    Code ++ encode(Rest, Table);
	false ->
	    io:format("not found ~w ~n", [C]),
	    encode(Rest, Table)
    end.


%%% decoding the sequence using the tree

decode(Seq, Tree) ->
    decode(Seq, Tree, Tree).

decode([], _, _) ->
     [];
decode([0|Seq], {Left, _}, Tree) ->
    decode(Seq, Left, Tree);
decode([1|Seq], {_, Right}, Tree) ->
    decode(Seq, Right, Tree);
decode(Seq, Char, Tree) ->
    [Char | decode(Seq, Tree, Tree)].



%% utf8 will read the content character by character where a character
%% might be two bytes (or more). The characters: å, ä and ö are two bytes.

base() -> lists:map(fun(C) -> {C, 0} end,
    [10,
           32,33,34,            39,
     40,41,      44,45,46,   48,49,
     50,51,52,53,      56,57,58,59,
     60,61,62,63,   65,66,67,68,69,
     70,71,72,73,74,75,76,77,78,79,
     80,81,82,83,84,85,86,      89,
                          97,98,99,
     100,101,102,103,104,105,106,107,108,109,
     110,111,112,    114,115,116,117,118,119,
     120,121,
             132,133,
     150,
     164,165,169,
     182,195,196,197,214,228,229,233,246]).


kallocain(N) ->
    {ok, Fd} = file:open("kallocain.txt", [read, binary]),
    {ok, Binary} = file:read(Fd, N),
    file:close(Fd),
    case unicode:characters_to_list(Binary, utf8) of
	{incomplete, List, _} ->
	    List;
	List ->
	    List
    end.
    
bench(N) ->
    Text = kallocain(N),
    L = length(Text),
    S = if
	    L > 1000 -> 1000;
	    true -> L
	end,
    {Sample, _} = lists:split(S, Text),
    Tree = tree(Sample),
    Table = table(Tree),
    {Seq, T3} = time(fun() -> encode(Text, Table) end),
    io:format("encoded in ~w ms~n", [T3 div 1000]),
    E = length(Seq) div 8,
    {_,T4} = time(fun() -> decode(Seq, Tree) end),
    io:format("decoded in ~w ms~n", [T4 div 1000]),
    io:format("source ~w bytes, endoded ~w bytes, ratio ~6.2f~n", [L, E, E/L]).

    
    
   
time(F) ->
    S = erlang:system_time(milli_seconds),
    R = F(),
    {R, erlang:system_time(milli_seconds) - S}.
