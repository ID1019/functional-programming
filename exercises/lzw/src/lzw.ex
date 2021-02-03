defmodule LZW do

  ## we need a known alphabet
  @alphabet 'abcdefghijklmnopqrstuvwxyz '

  ##  a demo sequence, only use charters in the alphabet

  def test(), do: test('luluulluululluluuuulululluuu')
  
  def test(text) do
    IO.puts("The string of text: #{text}\n")

    IO.puts("\nThe plain string as charlist:")
    IO.inspect(text, charlists: :as_lists)
    IO.puts("\nLength of text: #{length(text)}")

    table = table(@alphabet)
    
    IO.puts("\nEncoding")
    encoded = encode(text, table)

    IO.puts("\nThe encoded string as charlist:")
    IO.inspect(encoded, charlists: :as_lists)
    IO.puts("\nLength of encoded: #{length(encoded)}\n")

    IO.puts("\nDecoding")
    decoded = decode(encoded, table)
	
    case decoded do
      ^text ->
        IO.puts("\nSUCCESS: The decoded message matches the original string!\n")
      _ ->
        IO.puts("\nERROR: The decoded message does NOT match the original string!\n")
    end
  end

  ## Encode the character sequence given a table.

  def encode([], _), do: []
  def encode([char | rest], table) do
    word = [char]
    {:found, code} = encode_word(word, table)
    encode(rest, word, code, table)
  end

  ## Encode a list of characters given a word that you have seen
  ## sofar, its code and the table of codes.
    
  def encode([], _sofar, code, _table), do: [code]
  def encode([char | rest], sofar, code, table) do

    ## extend the word you have seen with the character
    extended = sofar ++ [char]

    ## if you're lucky it's in the table
    case encode_word(extended, table) do
      {:found, ext} ->
	## yes, continue you might be lucky again
        encode(rest, extended, ext, table)

      {:notfound, updated} ->
	## no, settle for the code you have and start over,
	## note that the table has been updated with a code
	## for the extended word
	sofar = [char]
	## this will allways succeed since sofar is a single char word
        {_, cd} = encode_word(sofar, updated)
        [code | encode(rest, sofar, cd, updated)]
    end
  end

  ## Find the code of a word if it is in the table otherwise add it to
  ## the table.
  
  def encode_word(word, table) do
    case lookup_code(table, word) do
      {_, code} ->
	## if found we return the code
        {:found, code}
      nil ->
	## otherwise we update the table
        {:notfound, add(table, word)}
    end
  end

  
  ## Decode a sequence of codes given a code table.

  def decode([], _), do: []
  
  def decode([code], table) do
    ## this is the last code in the sequence
    {word, _code} = lookup_word(table, code)
    IO.puts("out: #{word}")
    word
  end

  def decode([code | codes], table) do
    {word, _} = lookup_word(table, code)
    updated = decode_update(hd(codes), word, table)
    IO.puts("out: #{word}")
    word ++ decode(codes, updated)
  end

  ## Update the table by adding the word we have found exteded with
  ## what should have been the next character read by the encoder.
  
  def decode_update(next, word, table) do
    char = case lookup_word(table, next) do
	     {found, _} ->
	       hd(found)
	     nil ->
               IO.puts("Could not find the code #{next}")
               hd(word)
	   end
    add(table, word++[char])
  end
  

  ## The table is a simple maping from encoded words to codes. We keep
  ## track of the next code to assign to a word that we add.

  def table(alphabet) do
    n = length(alphabet)
    words = Enum.map(alphabet, fn x -> [x] end)
    codes = Enum.to_list(1..n)
    map = List.zip([words, codes])
    next = n + 1
    {next, map}
  end

  def lookup_code({_, words}, word) do
    List.keyfind(words, word, 0)
  end
  def lookup_word({_, words}, code) do
    List.keyfind(words, code, 1)
  end

  def add({n, words}, word) do
    IO.puts("Adding #{word} as code #{n}")
    {n+1, [{word, n}|words]}
  end
  
  

end
