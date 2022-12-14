defmodule Bench do
  
  def bench() do bench(100) end

  def bench(n) do

    ls = [16,32,64,128,256,512,1024,2*1024,4*1024,8*1024]

    time = fn (i, map, f) ->
      seq = Enum.map(1..i, fn(_) -> :rand.uniform(i) end)
      :timer.tc(fn () -> Enum.reduce(1..n, map, fn(_, map) -> f.(seq, map) end) end)
    end

    bench = fn (i) ->

      list_add= fn (seq, map) ->
        Enum.reduce(seq, map, fn (e, acc) -> EnvList.add(acc, e, :foo) end)
      end

      tree_add = fn (seq, map) ->
        Enum.reduce(seq, map, fn (e, acc) -> EnvTree.add(acc, e, :foo) end)
      end

      list_lookup= fn (seq, map) ->
        Enum.each(seq, fn (k) -> EnvList.lookup(map, k) end)
	map
      end

      tree_lookup = fn (seq, map) ->
        Enum.each(seq, fn (k) -> EnvTree.lookup(map, k) end)
	map
      end            

      list_remove = fn (seq, map) ->
        Enum.reduce(seq, map, fn (k, acc) -> EnvList.remove(acc, k) end)
      end

      tree_remove = fn (seq, map) ->
        Enum.reduce(seq, map, fn (k, acc) -> EnvTree.remove(acc, k) end)
      end            
      
      {tla, map_list} = time.(i, EnvList.new(), list_add)
      {tta, map_tree} = time.(i, EnvTree.new(), tree_add)

      {tll, _} = time.(i, map_list, list_lookup)
      {ttl, _} = time.(i, map_tree, tree_lookup)

      {tlr, _} = time.(i, map_list, list_remove)
      {ttr, _} = time.(i, map_tree, tree_remove)      

      :io.format("~6.w~14.2f~12.2f~16.2f~12.2f~16.2f~12.2f\n", [i,tla/(i*n), tta/(i*n), tll/(i*n), ttl/(i*n), tlr/(i*n), ttr/(i*n)])
    end

    :io.format("# benchmark of map as a list and as a tree (loop: ~w) \n", [n])
    :io.format("~7.s~13.s~28.s~28.s\n", ["n ", "add", "lookup", "remove"])
    :io.format("~20.s~12.s~16.s~12.s~16.s~10.s\n", ["list", "tree", "list", "tree", "list", "tree"])
    Enum.map(ls, fn (i) -> bench.(i) end)
    :ok
  end
  

  

end

