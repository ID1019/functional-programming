defmodule Data do

  def new() do
    new([])
  end    

  def new(segments) do
    f = fn({start, data}, layout) ->
      last = start +  length(data) -1      
      Enum.zip(start..last, data) ++ layout
    end
    layout = List.foldr(segments, [], f)
    {:data, Map.new(layout)}
  end

  def read({:data, data}, i) do
    Map.get(data, i)
  end

  def write({:data, data}, i, val) do
    {:data, Map.put(data, i, val)}
  end  

  


  
end
