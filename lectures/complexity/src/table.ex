defmodule Table do

  ## a tuple of n elements, 0 indexed

  def new(l) do List.to_tuple(l) end

  def lookup(tuple, k) do
    elem(tuple, k)
  end

  def modify(tuple, k, v) do
    put_elem(tuple, k, v)
  end

end



