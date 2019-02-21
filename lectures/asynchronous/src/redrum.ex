defmodule Redrum do 

  def new(list) do
    cells = Enum.map(list, fn(e) -> Asynch.start(e) end)
    {:mem, List.to_tuple(cells)}
  end

  def remote(node, list) do
    cells = Enum.map(list, fn(e) -> Asynch.remote(node, e) end)
    {:mem, List.to_tuple(cells)}
  end

  
  def read({:mem, mem}, n) do
    cell = elem(mem, n)
    Asynch.read(cell)
  end

  def collect(ref) do
    receive do
      {:value, ^ref, val} ->
	val
    end
  end

  def read_synch({:mem, mem}, n) do
    cell = elem(mem, n)
    ref = Asynch.read(cell)
    receive do
      {:value, ^ref, val} ->
	val
    end
  end  

  def write({:mem, mem}, n, val) do
    cell = elem(mem, n)
    Asynch.write(cell, val)
  end

  def write_synch({:mem, mem}, n, val) do
    cell = elem(mem, n)
    Asynch.write_synch(cell, val)
  end  

  def delete({:mem, mem}) do
    Enum.each(Tuple.to_list(mem), fn(c) -> Asynch.quit(c) end)
  end


end
