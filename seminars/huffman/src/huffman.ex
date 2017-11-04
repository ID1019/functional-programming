defmodule Huffman do

  # The sample text must conatain alla characters that we want to
  # encode.

  def sample() do
    'the quick brown fox jumps over the lazy dog
    this is a sample text that we will use when we build
    up a table we will only handle lower case letters and
    no punctuation symbols the frequency will of course not
    represent english but it is probably not that far off'
  end

  def text() do
    'this is something that we should encode'
  end

  def test() do
    sample = sample()
    tree(sample)
    # encode = encode_table(tree)
    # decode = decode_table(tree)
    # text = text()
    # seq = encode(text, encode)
    # text = decode(seq, decode)
  end


  # Building the Huffman tree, first get the frequencies and then
  # build teh tree.

  def tree(sample) do
    freq(sample)
    |> huffman
  end

  def freq(sample), do: freq(sample, [])
  def freq([], freq), do: freq
  def freq([char | rest], freq), do: freq(rest, update(char, freq))

  def update(char, []), do: [{char, 1}]
  def update(char, [{char, n} | freq]) do
    [{char, n + 1} | freq]
  end
  def update(char, [elem | freq]) do
    [elem | update(char, freq)]
  end


  # Now we build the tree.

  def huffman(freq) do
    Enum.sort(freq, fn ({_, x}, {_, y}) -> x < y end)
    |> huffman_tree
  end

  def huffman_tree([{tree, _}]), do: tree
  def huffman_tree([{a, af}, {b, bf} | rest]) do
    huffman_tree(insert({{a, b}, af + bf}, rest))
  end

  def insert({a, af}, []), do: [{a, af}]
  def insert({a, af}, [{b, bf} | rest]) when af < bf do
    [{a, af}, {b, bf} | rest]
  end
  def insert({a, af}, [{b, bf} | rest]) do
    [{b, bf} | insert({a, af}, rest)]
  end

# %% Build the encode table

# encode_table(Tree) ->  
#     codes(Tree, []). 
# %%  codes_better(Tree, [], []).

# codes({A,B}, Sofar) -> 
#     As = codes(A, [0|Sofar]), 
#     Bs = codes(B, [1|Sofar]), 
#     As ++ Bs; 
# codes(A, Code) -> 
#     [{A, lists:reverse(Code)}].


# codes_better({A,B}, Sofar, Acc) ->
#     Left = codes_better(A, [0|Sofar], Acc), 
#     codes_better(B, [1|Sofar], Left);
# codes_better(A, Code, Acc) -> 
#     [{A, lists:reverse(Code)} | Acc].

# %% The encoder      

# encode([], _Table) -> []; 
# encode([C|Rest], Table) -> 
#     {C, Code} = lists:keyfind(C, 1, Table), 
#     Code ++ encode(Rest, Table).


# %%% The decoder using the list of codes

# decode_table(Tree) ->
#     codes(Tree, []).

# decode([], _Table) ->
#      [];
# decode(Seq, Table) ->
#      {Char, Rest} = decode_char(Seq, 1, Table),
#      [Char|decode(Rest, Table)].

# decode_char(Seq, N, Table) ->
#      {Code, Rest} = lists:split(N, Seq),
#      case lists:keyfind(Code, 2, Table) of
#        {Char, Code} ->
#              {Char, Rest};
#        false ->
#              decode_char(Seq, N+1, Table)
#       end.

# %%% The decoder using the tree.

# %% decode_table(Tree) ->
# %%     Tree.

# %% decode(Seq, Tree) ->
# %%     decode(Seq, Tree, Tree).

# %% decode([], Char, _) ->
# %%      [Char];
# %% decode([0|Seq], {Left, _}, Tree) ->
# %%     decode(Seq, Left, Tree);
# %% decode([1|Seq], {_, Right}, Tree) ->
# %%     decode(Seq, Right, Tree);
# %% decode(Seq, Char, Tree) ->
# %%     [Char | decode(Seq, Tree, Tree)].
    
# %%% Get a suitable chunk of text to encode.

# kallocain(N,  Coding) ->
#     {ok, Fd} = file:open("kallocain.txt", [read, binary]),
#     {ok, Binary} = file:read(Fd, N),
#     file:close(Fd),
#     L = byte_size(Binary),
#     character_decode(Binary, L, Coding).

# %% latin1 will force to read one byte at a time

# %% utf8 will read the content character by character where a character
# %% might be two bytes (or more). The characters: å, ä and ö are two bytes.

# %% utf16 will read the content two bytes at a time (possibly more).

# %% utf24 is a faked coding scheme that will simply read three bytes at a time.

# character_decode(Binary, L, latin1) ->
#     {binary_to_list(Binary), L};
# character_decode(Binary, L, utf8) ->
#     {unicode:characters_to_list(Binary, utf8), L};
# character_decode(Binary, L, utf16) ->
#     case unicode:characters_to_list(Binary, utf16) of
# 	{incomplete, List, Rest} ->
# 	    {List, L - byte_size(Rest)};
# 	List ->
# 	    {List, L}
#     end;
# character_decode(Binary, L, utf24) ->
#     {bytes_to_list(Binary, 24), L - (L rem 3)}.

# bytes_to_list(Binary, B) ->
#     case Binary of
# 	<<Char:B, Bin/binary>> ->
# 	    [Char|bytes_to_list(Bin, B)];
#         <<_/binary>> ->
# 	    []
#     end.


# %%% This is the benchmark 

# bench(N, Coding) ->
#     {Sample, _} = kallocain(N, Coding),

#     {{Text, B}, T1} = time(fun() -> kallocain(N, Coding) end),

#     C = length(Text),

#     {Tree, T2} = time(fun() -> tree(Sample) end),

#     {Encode, T3} = time(fun() -> encode_table(Tree) end),

#     S = length(Encode),

#     {Decode, _} = time(fun() -> decode_table(Tree) end),

#     {Encoded, T5} = time(fun() -> encode(Text, Encode) end),

#     E = length(Encoded) div 8,

#     R = E/B,

#     {_, T6} = time(fun() -> decode(Encoded, Decode) end),

#     io:format(" read in ~w ms~n", [T1]),
#     io:format(" text of ~w characters~n", [C]),
#     io:format(" tree built in ~w ms~n", [T2]),
#     io:format(" table of size ~w in ~w ms~n", [S, T3]),
#     io:format(" encoded in ~w ms~n", [T5]),
#     io:format(" decoded in ~w ms~n", [T6]),
#     io:format(" source ~w bytes, encoded ~w bytes, compression ~.2f~n", [B, E, R]).


# time(F) ->
#     S = erlang:system_time(milli_seconds),
#     R = F(),
#     {R, erlang:system_time(milli_seconds) - S}.
end