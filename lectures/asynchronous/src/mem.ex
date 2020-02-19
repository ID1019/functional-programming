defmodule Mem do


  def new(list) do
    {:mem, List.to_tuple(list)}
  end

  def read({:mem, mem}, n) do
    elem(mem, n)
  end
    
  def write({:mem, mem}, n, val) do
    {:mem, put_elem(mem, n, val)}
  end

end
