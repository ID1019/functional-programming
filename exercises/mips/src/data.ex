defmodule Data do

  def new() do
    {:data, Map.new}
  end

  def read({:data, data}, i) do
    Map.get(data, i)
  end

  def write({:data, data}, i, val) do
    {:data, Map.put(data, i, val)}
  end  
  
end
