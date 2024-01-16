defmodule Leftist do

  ## a leftist tree is a heap with the added properties
  ##
  ##     Each node holds s  - the distance to the nearest empty branch (:nil)
  ##
  ##     The distance of the right branch is always smaller or equal to the
  ##     distance of its left branch. 
  ##
  ##      ... ergo, the left branch is always larger

  def new(less) do
    {:leftist, :nil, less}
  end

  def empty?({:leftist, :nil, _}) do true end
  def empty?(_) do false end  

  
  def enqueue({:leftist, :nil, less}, k, val) do
    {:leftist, {:node, k, val, 0, :nil, :nil}, less}
  end
  def enqueue({:leftist, node, less}, k, val) do
    {:leftist, merge({:node, k, val, 0, :nil, :nil}, node, less), less}
  end  

  def dequeue({:leftist, :nil, _}) do
    :nil
  end
  def dequeue({:leftist, {:node, k, val, 0, :nil, :nil}, less}) do
    {k, val, {:leftist, :nil, less}}
  end  
  def dequeue({:leftist, {:node, k, val, _, left, :nil}, less}) do
    {k, val, {:leftist, left, less}}
  end    
  def dequeue({:leftist, {:node, k, val, _, left, right}, less}) do
    {k, val, {:leftist, merge(left, right, less), less}}
  end      


  ## It's the merge function that does the work.
  
  def merge(left, :nil, _less)  do left end
  def merge(:nil, right, _less)  do right end
  
  def merge({:node, lk, lv, _, ll, lr}=left, {:node, rk, _, _, _, _} = right, less ) do
    if ( !less.(rk, lk) ) do
      merged = merge(lr, right, less)
      sm =  rank(merged)
      sl = rank(ll)
      s = min(sm, sl) + 1
      if ( sm < sl) do
	{:node, lk, lv, s, ll, merged}
      else
	{:node, lk, lv, s, merged, ll}
      end
    else
      merge(right, left, less)      
    end
  end


  def rank(:nil) do 0 end  
  def rank({:node, _, _, s, _, _}) do s end

end
