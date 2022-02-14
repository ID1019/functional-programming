defmodule Freq do

  def freq(n) do
    all = File.stream!("kallocain.txt") |>
      Stream.map(fn x -> String.trim(x, "\n") end) |>
      Stream.map(fn x -> String.downcase(x) end) |>
      Stream.map(fn x -> String.to_charlist(x) end) |>
      Stream.map(fn x -> remove(x) end) |>
      Stream.map(fn x -> split(x) end) |>
      Enum.reduce(Map.new(),
	fn (words, acc) ->
	  Enum.reduce(words, acc,  fn (x,acc) -> Map.update(acc, x, 1, fn n -> n + 1 end)  end )
	end)
    all = Map.to_list(all)
    all = Enum.sort(all, fn({_, v1}, {_,v2}) -> v1 > v2 end)
    Enum.take(all, n)
  end

  def split(str) do split(str, []) end
  
  def split([], []) do [] end
  def split([], word) do [Enum.reverse(word)] end  
  def split([32,32|str], word) do
    split([32|str], word)
  end
  def split([32|str], []) do
    split(str, [])
  end
  def split([32|str], word) do
    [Enum.reverse(word) | split(str, [])]
  end
  def split([c|str], word) do
    split(str, [c|word])
  end  

  
  def remove([]) do [] end
  def remove([c | str] ) do
    if Enum.member?([?., ?, , ?\" , ?;,  ?:,  ?-, ?1,  ?2,  ?3,  ?4,  ?5,  ?6,  ?7,  ?8,  ?9,  ?0], c)  do
      remove(str)
    else
      [c  | remove(str)]
    end
  end
  
  

end
