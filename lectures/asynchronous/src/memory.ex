defmodule Memory do

  def new(list) do
    cells = Enum.map(list, fn(e) -> Cell.start(e) end)
    {:mem, List.to_tuple(cells)}
  end

  def remote(node, list) do
    cells = Enum.map(list, fn(e) -> Cell.remote(node, e) end)
    {:mem, List.to_tuple(cells)}
  end

  
  def read({:mem, mem}, n) do
    cell = elem(mem, n)
    Cell.read(cell)
  end

  def write({:mem, mem}, n, val) do
    cell = elem(mem, n)
    Cell.write(cell, val)
  end

  def delete({:mem, mem}) do
    Enum.each(Tuple.to_list(mem), fn(c) -> Cell.quit(c) end)
  end
    
end
