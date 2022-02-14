defmodule Freq do

  def freq(n) do
    freq = File.stream!("kallocain.txt") |>
      Stream.map(fn x -> String.trim(x, "\n") end) |>
      Stream.map(fn x -> String.downcase(x) end) |>
      Stream.map(fn x -> String.to_charlist(x) end) |>
      Stream.map(fn x -> remove(x) end) |>
      Stream.map(fn x -> split(x) end) |>
      Enum.reduce(new(),
	fn (words, acc) ->
	  Enum.reduce(words, acc,  fn (x,acc) -> update(acc, x)  end )
	end)
    lst = to_list(freq)
    srt = Enum.sort(lst, fn({_,v1},{_,v2}) -> v1 > v2 end)
    Enum.take(srt, n)
  end

  #def new() do Map.new() end
  #def update(map, word) do Map.update(map, word, 1, fn n -> n + 1 end) end
  #def to_list(map) do Map.to_list(map) end

  

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
    if Enum.member?([?', ?., ?, , ?\" , ??, ?!, ?(, ?), ?;,  ?:,  ?-, ?1,  ?2,  ?3,  ?4,  ?5,  ?6,  ?7,  ?8,  ?9,  ?0], c)  do
      remove(str)
    else
      [c  | remove(str)]
    end
  end

  # can we do our own tries 

  def new() do :nil end
  def to_list(tree) do to_list(tree, []) end
  def update(tree, word) do update_tree(tree, encode(word))  end

  def to_list(:nil, _) do [] end
  def to_list({:fq, 0, branches}, word) do
    Enum.reduce(0..29, [], fn(i,acc) -> to_list(elem(branches, i), [i|word]) ++ acc end)
  end  
  def to_list({:fq, f, :empty}, word) do
    [{decode(Enum.reverse(word)), f}]
  end
  def to_list({:fq, f, branches}, word) do
    [{decode(Enum.reverse(word)), f} | Enum.reduce(0..29, [], fn(i,acc) -> to_list(elem(branches, i), [i|word]) ++ acc end)]
  end    


  def update_tree(:nil, code) do 
    new_tree(code)
  end
  def update_tree({:fq, f, branches}, []) do
    {:fq, f+1, branches}
  end    
  def update_tree({:fq, f, :empty}, [c|code]) do
    updated = new_tree(code)
    {:fq, f, put_elem(empty(), c, updated)}
  end  
  def update_tree({:fq, f, branches}, [c|code]) do
    updated = update_tree(elem(branches, c), code)
    {:fq, f, put_elem(branches, c, updated)}
  end  

  def empty() do {:nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil, :nil}  end

  def new_tree([]) do 
    {:fq, 1, :empty}
  end
  def new_tree([c|code]) do 
    branch = new_tree(code)
    {:fq, 0, put_elem(empty(), c, branch)}
  end
  

  
  def encode(word) do
    Enum.map(word, fn (c) ->
      case c do
	228 -> 27
	229 -> 26
	233 -> 28
	246 -> 29
	a -> a - 97
      end
    end)
  end

  def decode(code) do
    Enum.map(code, fn (c) ->
      case c do
	26 -> 229
	27 -> 228
	28 -> 233
	29 -> 246
	c -> c + 97
      end
    end)
  end  
  
  

end
