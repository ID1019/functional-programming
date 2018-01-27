defmodule LZW do
  @alphabet 'abcdefghijklmnopqrstuvwxyz '

  def test(), do: test('this is a test to see if it works')

  def test(text) do
    IO.puts("The string of text: #{text}\n")
    IO.puts("The plain string as charlist:")
    IO.inspect(text, charlists: :as_lists)

    encoded = encode(text)
    IO.puts("\nThe encoded string as charlist:")
    IO.inspect(encoded)

    IO.puts("\nLength of text: #{length(text)}")
    IO.puts("Length of encoded: #{length(encoded)}\n")

    string_text = to_string(text)
    case to_string(decode(encoded)) do
      ^string_text ->
        IO.puts("SUCCESS: The decoded message matches the original string!\n")
      _ ->
        IO.puts("ERROR: The decoded message does NOT match the original string!\n")
    end
  end

  def encode([]), do: []
  def encode([word | rest]) do
    table = table()
    {:found, code} = encode_word(word, table)
    encode(rest, word, code, table)
  end

  def encode([], _sofar, code, _table), do: [code]
  def encode([word | rest], sofar, code, table) do
    extended = [word | sofar]

    case encode_word(extended, table) do
      {:found, ext} ->
        encode(rest, extended, ext, table)

      {:notfound, updated} ->
        {:found, cd} = encode_word(word, table)
        [code | encode(rest, word, cd, updated)]
    end
  end

  def encode_word(word, {n, words}) do
    case List.keyfind(words, word, 0) do
      {_word, code} ->
        {:found, code}

      nil ->
        {:notfound, {n + 1, [{word, n} | words]}}
    end
  end

  def decode(codes) do
    table = table()
    decode(codes, table)
  end

  def decode([], _), do: []

  def decode([code | []], {_, words}) do
    {word, _code} = List.keyfind(words, code, 1)
    word
  end

  def decode([code | codes], {n, words}) do
    {word, _} = List.keyfind(words, code, 1)
    [next | _] = codes

    next_char =
      case List.keyfind(words, next, 1) do
        {char, _} ->
          case is_list(char) do
            true -> List.first(char)
            false -> char 
          end

        nil ->
          IO.puts("Could not find #{next}, I bet it is #{n}!")
          [char | _] = word
          char
      end
  
    # IO.inspect("Adding #{[word]} and #{[next_char]} as code #{n}")
    [word] ++ [decode(codes, {n + 1, [{[word] ++ [next_char], n} | words]})]
  end

  def table do
    n = length(@alphabet)
    numbers = Enum.to_list(1..n)
    map = List.zip([@alphabet, numbers])
    {n + 1, map}
  end
end
