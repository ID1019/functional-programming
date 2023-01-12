defmodule Train do


  def take(_, 0) do [] end
  def take([h|t], n) when n > 0 do [h|take(t, n-1)] end  


  def drop(lst, 0) do lst end
  def drop([_|t], n) when n > 0 do drop(t, n-1) end  

  def append([], ys) do ys end
  def append([h|t], ys) do [h|append(t,ys)] end

  def member([],_) do false end
  def member([y|_], y) do true end
  def member([_|t], y) do member(t, y) end  

  def position([y|_], y) do 1 end
  def position([_|t], y) do position(t, y) + 1 end  
  
end
