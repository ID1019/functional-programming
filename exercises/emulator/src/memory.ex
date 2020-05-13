defmodule Memory do

  def new() do
    {:mem, Map.new()}
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

  
