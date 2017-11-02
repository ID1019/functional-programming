-module(huffman).

-compile(export_all).

%%% The sample text must conatain alla characters that we want to
%%% encode.

sample() -> "the quick brown fox jumps over the lazy dog
     this is a sample text that we will use when we build 
     up a table we will only handle lower case letters and
     no punctuation symbols the frequency will of course not 
     represent english but it is probably not that far off".

text() -> "this is something that we should encode".

test() ->
    Sample = sample(),
    Tree = tree(Sample),
    Encode = encode_table(Tree),
    Decode = decode_table(Tree),
    Text = text(),
    Seq = encode(Text, Encode),
    decode(Seq, Decode).

%%% Building the Huffman tree, first get the frequencies and then
%%% build teh tree.
 
tree(Sample) -> 
    Freq = freq(Sample),
    huffman(Freq).

freq(Sample) -> 
    freq(Sample, []).

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

%%% Now we build the tree.

huffman(Freq) ->
    Sorted = lists:sort(fun({_,X}, {_,Y}) -> X < Y end, Freq),
    huffman_tree(Sorted).

huffman_tree([{Tree, _}]) ->
    Tree;
huffman_tree([{A,Af}, {B,Bf} | Rest]) ->
    huffman_tree(insert({{A,B}, Af+Bf}, Rest)).

insert({A,Af}, []) ->
    [{A,Af}];
insert({A,Af}, [{B,Bf}|Rest]) when Af < Bf ->
    [{A,Af}, {B,Bf} | Rest];
insert({A,Af}, [{B,Bf}|Rest]) ->
    [{B,Bf} | insert({A,Af}, Rest)].

%% Build the encode table

encode_table(Tree) ->  
    codes(Tree, []). 
%%  codes_better(Tree, [], []).

codes({A,B}, Sofar) -> 
    As = codes(A, [0|Sofar]), 
    Bs = codes(B, [1|Sofar]), 
    As ++ Bs; 
codes(A, Code) -> 
    [{A, lists:reverse(Code)}].


codes_better({A,B}, Sofar, Acc) ->
    Left = codes_better(A, [0|Sofar], Acc), 
    codes_better(B, [1|Sofar], Left);
codes_better(A, Code, Acc) -> 
    [{A, lists:reverse(Code)} | Acc].

%% The encoder      

encode([], _Table) -> []; 
encode([C|Rest], Table) -> 
    {C, Code} = lists:keyfind(C, 1, Table), 
    Code ++ encode(Rest, Table).


%%% The decoder using the list of codes

decode_table(Tree) ->
    codes(Tree, []).

decode([], _Table) ->
     [];
decode(Seq, Table) ->
     {Char, Rest} = decode_char(Seq, 1, Table),
     [Char|decode(Rest, Table)].

decode_char(Seq, N, Table) ->
     {Code, Rest} = lists:split(N, Seq),
     case lists:keyfind(Code, 2, Table) of
       {Char, Code} ->
             {Char, Rest};
       false ->
             decode_char(Seq, N+1, Table)
      end.

%%% The decoder using the tree.

%% decode_table(Tree) ->
%%     Tree.

%% decode(Seq, Tree) ->
%%     decode(Seq, Tree, Tree).

%% decode([], Char, _) ->
%%      [Char];
%% decode([0|Seq], {Left, _}, Tree) ->
%%     decode(Seq, Left, Tree);
%% decode([1|Seq], {_, Right}, Tree) ->
%%     decode(Seq, Right, Tree);
%% decode(Seq, Char, Tree) ->
%%     [Char | decode(Seq, Tree, Tree)].
    
%%% Get a suitable chunk of text to encode.

kallocain(N,  Coding) ->
    {ok, Fd} = file:open("kallocain.txt", [read, binary]),
    {ok, Binary} = file:read(Fd, N),
    file:close(Fd),
    L = byte_size(Binary),
    character_decode(Binary, L, Coding).

%% latin1 will force to read one byte at a time

%% utf8 will read the content character by character where a character
%% might be two bytes (or more). The characters: å, ä and ö are two bytes.

%% utf16 will read the content two bytes at a time (possibly more).

%% utf24 is a faked coding scheme that will simply read three bytes at a time.

character_decode(Binary, L, latin1) ->
    {binary_to_list(Binary), L};
character_decode(Binary, L, utf8) ->
    {unicode:characters_to_list(Binary, utf8), L};
character_decode(Binary, L, utf16) ->
    case unicode:characters_to_list(Binary, utf16) of
	{incomplete, List, Rest} ->
	    {List, L - byte_size(Rest)};
	List ->
	    {List, L}
    end;
character_decode(Binary, L, utf24) ->
    {bytes_to_list(Binary, 24), L - (L rem 3)}.

bytes_to_list(Binary, B) ->
    case Binary of
	<<Char:B, Bin/binary>> ->
	    [Char|bytes_to_list(Bin, B)];
        <<_/binary>> ->
	    []
    end.


%%% This is the benchmark 

bench(N, Coding) ->
    {Sample, _} = kallocain(N, Coding),

    {{Text, B}, T1} = time(fun() -> kallocain(N, Coding) end),

    C = length(Text),

    {Tree, T2} = time(fun() -> tree(Sample) end),

    {Encode, T3} = time(fun() -> encode_table(Tree) end),

    S = length(Encode),

    {Decode, _} = time(fun() -> decode_table(Tree) end),

    {Encoded, T5} = time(fun() -> encode(Text, Encode) end),

    E = length(Encoded) div 8,

    R = E/B,

    {_, T6} = time(fun() -> decode(Encoded, Decode) end),

    io:format(" read in ~w ms~n", [T1]),
    io:format(" text of ~w characters~n", [C]),
    io:format(" tree built in ~w ms~n", [T2]),
    io:format(" table of size ~w in ~w ms~n", [S, T3]),
    io:format(" encoded in ~w ms~n", [T5]),
    io:format(" decoded in ~w ms~n", [T6]),
    io:format(" source ~w bytes, encoded ~w bytes, compression ~.2f~n", [B, E, R]).


time(F) ->
    S = erlang:system_time(milli_seconds),
    R = F(),
    {R, erlang:system_time(milli_seconds) - S}.
