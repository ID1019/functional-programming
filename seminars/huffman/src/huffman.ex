defmodule Huffman do

  # The sample text must conatain alla characters that we want to
  # encode

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
    tree = tree(sample)
    encode = encode_table(tree)
    decode = decode_table(tree)
    text = text()
    seq = encode(text, encode)
    decode(seq, decode)
  end


  # Building the Huffman tree, first get the frequencies and then
  # build teh tree

  def tree(sample) do
    freq(sample)
    |> huffman()
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


  # Now we build the tree

  def huffman(freq) do
    Enum.sort(freq, fn ({_, x}, {_, y}) -> x < y end)
    |> huffman_tree()
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


# Build the encode table

def encode_table(tree) do
  codes(tree, [])
  # codes_better(tree, [], [])
end

def codes({a, b}, sofar) do
  as = codes(a, [0 | sofar])
  bs = codes(b, [1 | sofar])
  as ++ bs; 
end
def codes(a, code) do
  [{a, Enum.reverse(code)}]
end

def codes_better({a, b}, sofar, acc) do
  left = codes_better(a, [0 | sofar], acc) 
  codes_better(b, [1 | sofar], left)
end
def codes_better(a, code, acc) do
  [{a, Enum.reverse(code)} | acc]
end


# The encoder      

def encode([], _), do: []
def encode([char | rest], table) do
  {_, code} = List.keyfind(table, char, 0)
  code ++ encode(rest, table)
end


# The decoder using the list of codes

def decode_table(tree), do: codes(tree, [])

def decode([], _), do: []
def decode(seq, table) do
  {char, rest} = decode_char(seq, 1, table)
  [char | decode(rest, table)]
end

def decode_char(seq, n, table) do
  {code, rest} = Enum.split(seq, n)

  case List.keyfind(table, code, 1) do
    {char, _} ->
      {char, rest};
    nil ->
      decode_char(seq, n + 1, table)
  end
end


# The decoder using the tree.

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
    

# Get a suitable chunk of text to encode.

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