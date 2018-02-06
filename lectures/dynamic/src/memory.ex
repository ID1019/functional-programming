defmodule Memory do

  @moduledoc """
  This module will handle a key value memory.
  """

  @doc """
  new/0 will create a new memory
  """
  
  def new() do  [] end
    
  def store(k,v, mem) do
    [{k,v}|mem]
  end
    
  def lookup(k, mem) do
    List.keyfind(mem, k, 0)
  end

end


