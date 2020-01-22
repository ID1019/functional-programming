defmodule Tree do

  def list_insert(e, []) do
    [e]
  end
  def list_insert(e, [h|t]) when e < h do
    [e,h|t]
  end
  def list_insert(e, [h|t]) do
    [h|list_insert(e, t)]
  end

  def tree_insert(e, :nil) do {:leaf, e} end
  def tree_insert(e, {:leaf, h}=right) when e < h do
    {:node, e, :nil, right}
  end
  def tree_insert(e, {:leaf, _}=left)  do
    {:node, e, left, :nil}
  end  
  def tree_insert(e, {:node, h, left, right}) when e < h do
    {:node, h, tree_insert(e, left), right}
  end
  def tree_insert(e, {:node, h, left, right}) do
    {:node, h, left, tree_insert(e, right)}
  end  
  

  def bench() do

    ls = [10,20,40,80,160,320,640,1280]

    test = fn (i, f) ->
      seq = sequence(i, 10000)
      elem(:timer.tc(fn () -> f.(seq) end),0)
    end
    IO.write("# benchmark of lists and tree \n")

    bench = fn (i) ->
      list = fn (seq) ->
	List.foldr(seq, [], fn (e, acc) -> list_insert(e, acc) end)
      end
      tree = fn (seq) ->
	List.foldr(seq, :nil, fn (e, acc) -> tree_insert(e, acc) end)
      end      
      tl = test.(i, list) 
      tt = test.(i, tree)     
      IO.write("  #{tl}\t\t#{tt}\n")
    end
    
    Enum.map(ls, bench)
    :ok
  end
  
  def sequence(0, _) do
    []
  end
  def sequence(i, t) do
    [:rand.uniform(t) | sequence(i - 1, t)]
  end
  

end
