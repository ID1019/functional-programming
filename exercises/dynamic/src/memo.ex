defmodule Memo do


  def new() do [] end

  def lookup([], _) do :nil end
  def lookup([{:tr, k, val, _}|_], [k]) do val end
  def lookup([{:tr, k, _, mem}|_], [k|ks]) do lookup(mem, ks) end
  def lookup([_|rest], ks) do lookup(rest, ks) end  

  def add([], [k], v) do [{:tr, k, v, []}] end 
  def add([{:tr, k, _, mem}|rest], [k], v) do [{:tr, k, v, mem}|rest] end
  def add([{:tr, k, val, mem}|rest], [k|ks], v) do [{:tr, k, val, add(mem, ks, v)}|rest] end
  def add([tr|rest], ks, v) do [tr | add(rest, ks, v)] end  
  

end

