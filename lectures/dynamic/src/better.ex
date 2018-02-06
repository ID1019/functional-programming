defmodule Better do

  @moduledoc """
  The memory module bbut now using a map.
  """

  @doc """
  new/0 will create a new memory
  """
  
  def new() do  %{} end
    
  def store(k,v, mem) do
    Map.put(mem, k, v)
  end
    
  def lookup(k, mem) do
    Map.get(mem, k)
  end

end


