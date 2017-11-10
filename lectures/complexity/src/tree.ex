defmodule Tree do

  def empty() do :nil end

  def lookup(:nil, _) do false end

  def lookup({:node, k, v, _, _}, k) do v end

  def lookup({:node, l, _, left, right}, k) do 
    if k < l do
      lookup(left, k)
    else
      lookup(right, k)
    end
  end

  def modify(:nil, _, _) do :nil end
  def modify({:node, k, _, left, right}, k, v) do 
    {:node, k, v, left, right}
  end
  def modify({:node, l, u, left, right}, k, v) do 
    if k < l do
      {:node, l, u, modify(left, k, v), right}
    else
      {:node, l, u,  left, modify(right, k, v)}
    end
  end
    
  def insert(:nil, k, v) do
    {:node, k, v, :nil, :nil}
  end
  def insert({:node, k, _, left, right}, k, v) do
    {:node, k, v, left, right}
  end
  def insert({:node, l, u, left, right}, k, v) do 
    if k < l do
      {:node, l, u, insert(left, k, v), right}
    else
      {:node, l, u,  left, insert(right, k, v)}
    end
  end

  def delete( :nil, _) do :nil end
  def delete({:node, k, _, node, :nil}, k) do  node end
  def delete({:node, k, _, :nil, node}, k) do  node end
  def delete({:node, k, _, left, right}, k) do 
    {u, l, removed} = lift(left)
    {:node, u, l, removed, right}
  end
  def delete({:node, u, l, left, right}, k) do 
    if k < u do
      {:node, u, l, delete(left, k), right}
    else
      {:node, u, l, left, delete(right, k)}
    end
  end
    
  def lift({:node, k, v, left, :nil}) do {k, v, left} end
  def lift({:node, k, v, left, right}) do  
    {u, l, removed} = lift(right)
    {u, l, {:node, k, v, left, removed}}
  end

end 
