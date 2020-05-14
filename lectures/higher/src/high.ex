defmodule High do 


  def sort([]) do [] end
  def sort([n]) do [n] end
  def sort(list) do
    {a, b} = split(list)
    merge(sort(a), sort(b))
  end

  def merge([], b) do b end
  def merge(a, []) do a end
  def merge([ha|ta], [hb|_]=b) when ha < hb do
    [ha | merge(ta, b)]
  end
  def merge(a, [hb|tb])do
    [hb | merge(a, tb)]
  end  

  def split(list) do split(list, [], []) end

  def split([], a, b) do {a, b} end
  def split([h|t], a, b) do split(t, b, [h|a]) end

  
  
  def foldr([], acc, _op) do  acc end
  def foldr([h|t], acc, op) do 
    op.(h, foldr(t, acc, op))
  end

  def foldl([], acc, _op) do acc end
  def foldl([h|t], acc, op) do
    foldl(t, op.(h, acc), op)
  end
    




def appendr(l) do
  f = fn(e,a) -> e ++ a end
  foldr(l, [], f)
end


def appendl(l) do
  f = fn(e,a) -> a ++ e end
  foldl(l, [], f)
end





end
