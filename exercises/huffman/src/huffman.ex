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

  def text, do: 'this is something that we should encode'

  def test do
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
    freq = freq(sample)
    #huffman(freq)
  end

  def freq(sample), do: freq(sample, [])
  def freq([], freq), do: freq
  def freq([char | rest], freq), do: freq(rest, update(char, freq))

  def update(char, []), do: [{char, 1}]
  def update(char, [{char, n} | freq]), do: [{char, n + 1} | freq]
  def update(char, [elem | freq]), do: [elem | update(char, freq)]


  # Now we build the tree

  def huffman(freq) do
    sorted = Enum.sort(freq, fn ({_, x}, {_, y}) -> x < y end)
    huffman_tree(sorted)
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


  ## The decoder using the tree

  # def decode_table(tree), do: tree

  # def decode(seq, tree), do: decode(seq, tree, tree)
  # def decode([], char, _), do: [char]
  # def decode([0 | seq], {left, _}, tree) do
  #   decode(seq, left, tree)
  # end
  # def decode([1 | seq], {_, right}, tree) do
  #   decode(seq, right, tree)
  # end
  # def decode(seq, char, tree) do
  #   [char | decode(seq, tree, tree)]
  # end


  # Get a suitable chunk of text to encode

  def kallocain(n, coding) do
    {:ok, file} = File.open("kallocain.txt", [:read])
    binary = IO.read(file, n)
    File.close(file)

    length = byte_size(binary)
    character_decode(binary, length, coding)
  end


  # latin1 will force to read one byte at a time

  # utf8 will read the content character by character where a character
  # might be two bytes (or more). The characters: å, ä and ö are two bytes.

  # utf16 will read the content two bytes at a time (possibly more).

  # utf24 is a faked coding scheme that will simply read three bytes at a time.

  def character_decode(binary, length, :latin1) do
    {:binary.bin_to_list(binary), length}
  end
  def character_decode(binary, length, :utf8) do
    {:unicode.characters_to_list(binary, :utf8), length}
  end
  def character_decode(binary, length, :utf16) do
    case :unicode.characters_to_list(binary, :utf16) do
      {:incomplete, list, rest} ->
        {list, length - byte_size(rest)};
      list ->
        {list, length}
    end
  end


  # This is the benchmark

  def bench(n, coding) do
    {sample, _} = kallocain(n, coding)

    {{text, b}, t1} = time(fn -> kallocain(n, coding) end)

    c = length(text)

    {tree, t2} = time(fn -> tree(sample) end)

    {encode, t3} = time(fn -> encode_table(tree) end)

    s = length(encode)

    {decode, _} = time(fn -> decode_table(tree) end)

    {encoded, t5} = time(fn -> encode(text, encode) end)

    e = div(length(encoded), 8)

    r = Float.round(e / b, 3)

    {_, t6} = time(fn -> decode(encoded, decode) end)

    IO.puts(' read in #{t1} ms')
    IO.puts(' text of #{c} characters')
    IO.puts(' tree built in #{t2} ms')
    IO.puts(' table of size #{s} in #{t3} ms')
    IO.puts(' encoded in #{t5} ms')
    IO.puts(' decoded in #{t6} ms')
    IO.puts(' source #{b} bytes, encoded #{e} bytes, compression #{r}')
  end

  def time(func) do
    initial = Time.utc_now()
    result = func.()
    final = Time.utc_now()
    {result, Time.diff(final, initial, :microsecond) / 1000}
  end

end