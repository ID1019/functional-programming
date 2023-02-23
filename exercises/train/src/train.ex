defmodule Train do


  def take(_, 0) do [] end
  def take([h|t], n) when n > 0 do [h|take(t, n-1)] end  


  def drop(t, 0) do t end
  def drop([_|t], n) when n > 0 do drop(t, n-1) end  

  def append([], ys) do ys end
  def append([h|t], ys) do [h|append(t,ys)] end

  def member([],_) do false end
  def member([y|_], y) do true end
  def member([_|t], y) do member(t, y) end  

  def position([y|_], y) do 1 end
  def position([_|t], y) do position(t, y) + 1 end  

  def split([y|t], y) do  {[], t}  end
  def split([h|t], y) do
    {t, drop} = split(t, y)
    {[h|t], drop}
  end
  
  def main([], n) do {n, [], []} end
  def main([h|t], n) do 
      case main(t, n) do
	{0, drop, take} ->
	  {0, [h|drop], take}
	{n, drop, take} ->
	  {n-1, drop, [h|take]}
      end
  end
  
end

