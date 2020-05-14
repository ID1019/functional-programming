defmodule Memory do

  def new() do
    new([])
  end    

  def new(segments) do
    f = fn({start, data}, layout) ->
      last = start +  length(data) -1      
      Enum.zip(start..last, data) ++ layout
    end
    layout = List.foldr(segments, [], f)
    {:mem, Map.new(layout)}
  end

  def read({:mem, mem}, i) do
    case Map.get(mem, i) do
      nil -> 0
      val -> val
    end
  end

  def write({:mem, mem}, i, v) do
    {:mem, Map.put(mem, i, v)}
  end  

end

  
