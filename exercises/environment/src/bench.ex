defmodule Bench do
  
  def bench() do bench(1000) end

  def bench(n) do

    ls = [16,32,64,128,256,512,1024,2*1024,4*1024,8*1024]        
    :io.format("# benchmark of key-value store as a list, a tree and Map: ~w operations, time per operation in us\n", [n])
    :io.format("~6.s~8.s~-36.s~-36.s~-36.s\n", ["n", "", "add", "lookup", "remove"])
    :io.format("~18.s~12.s~12.s~12.s~12.s~12.s~12.s~12.s~12.s\n", ["list", "tree", "map", "list", "tree", "map", "list", "tree", "map"])
    Enum.each(ls, fn (i) ->
      {i, tla, tta, tma, tll, ttl, tml, tlr, ttr, tmr} = bench(i, n)
      :io.format("~6.w~12.2f~12.2f~12.2f~12.2f~12.2f~12.2f~12.2f~12.2f~12.2f\n", [i,tla/n, tta/n, tma/n, tll/n, ttl/n, tml/n, tlr/n, ttr/n, tmr/n])
    end)

    :ok
  end

  def bench(i, n) do 				    
    seq = Enum.map(1..i, fn(_) -> :rand.uniform(i) end)

    list = Enum.reduce(seq,  EnvList.new(),  fn(e, list) -> EnvList.add(list, e, :foo) end)
    tree = Enum.reduce(seq,  EnvTree.new(),  fn(e, tree) -> EnvTree.add(tree, e, :foo) end)
    map = Enum.reduce(seq,  Map.new(),  fn(e, map) -> Map.put(map, e, :foo)  end)    

    seq = Enum.map(1..n, fn(_) -> :rand.uniform(i) end)
    
    {tla, _} = :timer.tc(fn() -> Enum.each(seq, fn(e) -> EnvList.add(list, e, :foo) end) end)
    {tta, _} = :timer.tc(fn() -> Enum.each(seq, fn(e) -> EnvTree.add(tree, e, :foo) end) end) 
    {tma, _} = :timer.tc(fn() -> Enum.each(seq, fn(e) -> Map.put(map, e, :foo) end) end) 

    {tll, _} = :timer.tc(fn() -> Enum.each(seq, fn(e) -> EnvList.lookup(list, e) end) end) 
    {ttl, _} = :timer.tc(fn() -> Enum.each(seq, fn(e) -> EnvTree.lookup(tree, e) end) end) 
    {tml, _} = :timer.tc(fn() -> Enum.each(seq, fn(e) -> Map.get(map, e) end) end) 
    
    {tlr, _} = :timer.tc(fn() -> Enum.each(seq, fn(e) -> EnvList.remove(list, e) end) end) 
    {ttr, _} = :timer.tc(fn() -> Enum.each(seq, fn(e) -> EnvTree.remove(tree, e) end) end) 
    {tmr, _} = :timer.tc(fn() -> Enum.each(seq, fn(e) -> Map.delete(map, e) end) end) 
    
    {i, tla, tta, tma, tll, ttl, tml, tlr, ttr, tmr}
  end




  
end


