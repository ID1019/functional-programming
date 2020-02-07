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

  
  
  def foldr(_op, acc, []) do  acc end
  def foldr(op, acc, [h|t]) do 
    op.(h, foldr(op, acc, t))
  end

  def foldl(_op, acc, []) do acc end
  def foldl(op, acc, [h|t]) do
    foldl(op, op.(h, acc), t)
  end
    




def appendl(l) do
  f = fn(e,a) -> a ++ e end
  foldl(f, [], l)
end


def appendr(l) do
  f = fn(e,a) -> e ++ a end
  foldr(f, [], l)
end



end
