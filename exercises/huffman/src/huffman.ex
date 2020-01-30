defmodule Huffman do
  
  # The sample text must contain alla characters that we
  # want to encode.
  def sample() do
    'the quick brown fox jumps over the lazy dog
    this is a sample text that we will use when we build
    up a table we will only handle lower case letters and
    no punctuation symbols the frequency will of course not
    represent english but it is probably not that far off'
  end

  def text, do: 'this is something that we should encode'

  # Run all the steps of Huffman encoding and decoding.
  def test do
    sample = sample()
    tree = tree(sample)
    encode = encode_tuple(tree)
    decode = decode_table(tree)
    text = text()
    seq = encode_tuple(text, encode)
    decode(seq, decode)
  end

  # Construct the Huffman tree from a text sample.
  def tree(sample) do
    freq = freq(sample)
    huffman(freq)
  end


  
  # Compute the frequencies of all the characters in the
  # sample text and return a list of tuples {char, freq}.
  def freq(sample), do: freq(sample, [])

  def freq([], freq), do: freq
  def freq([char | rest], freq) do
    freq(rest, update(char, freq))
  end

  def update(char, []), do: [{char, 1}]
  def update(char, [{char, n} | freq]) do
    [{char, n + 1} | freq]
  end
  def update(char, [elem | freq]) do
    [elem | update(char, freq)]
  end










  
  # Build the actual Huffman tree inserting a character at
  # time based on the frequency.
  def huffman(freq) do
    sorted = Enum.sort(freq, fn({_, x}, {_, y}) -> x < y end)
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




  
  # Build the encoding table.
  def encode_table(tree) do
    #codes(tree, [])
    # codes_better(tree, [], [])
    Enum.sort( codes_better(tree, [], []), fn({_,x},{_,y}) -> length(x) < length(y) end)
  end

  # Traverse the Huffman tree and build a binary encoding
  # for each character.
  def codes({a, b}, sofar) do
    as = codes(a, [0 | sofar])
    bs = codes(b, [1 | sofar])
    as ++ bs
  end
  def codes( a, code) do
    [{a, Enum.reverse(code)}]
  end




  ## A better travering of the tree
  
  def codes_better({a, b}, sofar, acc) do
    left = codes_better(a, [0 | sofar], acc)
    codes_better(b, [1 | sofar], left)
  end
  def codes_better(a, code, acc) do
    [{a, Enum.reverse(code)} | acc]
  end



  ## An alternative way of representing the encode table.

  def encode_tuple(tree) do
    codes = codes(tree, [])
    sorted = Enum.sort(codes, fn({x,_}, {y,_}) ->  x < y end)
    extended = extend_codes(sorted, 0)
    List.to_tuple(extended)
  end

  def extend_codes([], _) do [] end
  def extend_codes([{n,code}|rest], n) do [code | extend_codes(rest, n+1)] end  
  def extend_codes(codes, n) do [ [] | extend_codes(codes, n+1)] end  
  
  def encode_tuple([], _), do: []
  def encode_tuple([char | rest], table) do
     code = elem(table, char)
     code ++ encode_tuple(rest, table)
  end

  ## An improvement where we do not waste any stack space
  
  #def encode_tuple(text, table) do
  #   encode_tuple(text, table, [])
  #end
  
  def encode_tuple([], _, acc) do flattenr(acc, []) end
  
  def encode_tuple([char | rest], table, acc) do
    code = elem(table, char)
    encode_tuple(rest, table, [code | acc])
  end

  def flattenr([], acc) do acc end
  def flattenr([code|rest], acc) do
    # this could further be improved if we didn't reverse the code 
    flattenr(rest, code ++ acc)
  end  

  # if code was stored in the reveresed order
  def add([], acc) do acc end
  def add([b|rest], acc) do   
    add(rest, [b|acc])
  end
  


  

  
  # Parse a string of text and encode it with the
  # previously generated encoding table.
  def encode([], _), do: []
  def encode([char | rest], table) do
    {_, code} = List.keyfind(table, char, 0)
    code ++ encode(rest, table)
  end


  



  
  # Decode a string of text using the same encoding
  # table as above. This is a shortcut and an
  # unrealistic situation.

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
        {char, rest}

      nil ->
        decode_char(seq, n + 1, table)
    end
  end






  

  # # The decoder using the tree. This is a more realistic
  # # solution.

   # def decode_table(tree) do tree end

   # def decode(seq, tree) do
   #   decode(seq, tree, tree)
   # end
  
  def decode([], char, _)  do
    [char]
  end
  
  def decode([0 | seq], {left, _}, tree) do
    decode(seq, left, tree)
  end
  def decode([1 | seq], {_, right}, tree) do
    decode(seq, right, tree)
  end
  def decode(seq, char, tree) do
    [char | decode(seq, tree, tree)]
  end

 
 
  # This is the benchmark of the single operations in the
  # Huffman encoding and decoding process.

  def bench(file, n) do
    {text, b} = read(file, n)
    c = length(text)
    {tree, t2} = time(fn -> tree(text) end)
    {encode, t3} = time(fn -> encode_table(tree) end)
    s = length(encode)
    {decode, _} = time(fn -> decode_table(tree) end)
    {encoded, t5} = time(fn -> encode(text, encode) end)
    e = div(length(encoded), 8)
    r = Float.round(e / b, 3)
    {_, t6} = time(fn -> decode(encoded, decode) end)

    IO.puts("text of #{c} characters")
    IO.puts("tree built in #{t2} ms")
    IO.puts("table of size #{s} in #{t3} ms")
    IO.puts("encoded in #{t5} ms")
    IO.puts("decoded in #{t6} ms")
    IO.puts("source #{b} bytes, encoded #{e} bytes, compression #{r}")
  end

  # Measure the execution time of a function.
  def time(func) do
    initial = Time.utc_now()
    result = func.()
    final = Time.utc_now()
    {result, Time.diff(final, initial, :microsecond) / 1000}
  end

 # Get a suitable chunk of text to encode.
  def read(file, n) do
   {:ok, fd} = File.open(file, [:read])
    binary = IO.read(fd, n)
    File.close(fd)

    length = byte_size(binary)
    case :unicode.characters_to_list(binary, :utf8) do
      {:incomplete, chars, rest} ->
        {chars, length - byte_size(rest)}
      chars ->
        {chars, length}
    end
  end
  


  def banch(file, n) do
    {text, b} = read(file, n)
    c = length(text)
    {tree, t2} = time(fn -> tree(text) end)
    {encode, t3} = time(fn -> encode_tuple(tree) end)
    s = tuple_size(encode)
    {decode, _} = time(fn -> decode_table(tree) end)
    {encoded, t5} = time(fn -> encode_tuple(text, encode) end)
    e = div(length(encoded), 8)
    r = Float.round(e / b, 3)
    {_, t6} = time(fn -> decode(encoded, decode) end)

    IO.puts("text of #{c} characters")
    IO.puts("tree built in #{t2} ms")
    IO.puts("table of size #{s} in #{t3} ms")
    IO.puts("encoded in #{t5} ms")
    IO.puts("decoded in #{t6} ms")
    IO.puts("source #{b} bytes, encoded #{e} bytes, compression #{r}")
  end


  

  

end



