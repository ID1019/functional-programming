defmodule Bench do

  def tree() do
    r = 100000
    :io.format("# Benchmark of tree operations, (~w runs), times in ms~n#~n", [r])
    :io.format("# n * 1000 is size of tree~n", [])
    :io.format("#~7s ~8s ~8s~n", ["n", "lookup", "modify"])

    ns = [1000,2000,4000,8000,16000,32000,64000,128000,256000,512000,1024000]

    bench = fn(n) ->
      tree = init_tree(n)		    
      ops = init_ops(r,n)
      tl  = time(fn() -> Enum.each(ops, fn(s) -> Tree.lookup(tree, s)     end) end)
      tm  = time(fn() -> Enum.each(ops, fn(s) -> Tree.modify(tree, s, :na) end) end)

      :io.format("~8w ~8w ~8w~n", [div(n,1000), tl, tm])
    end
    Enum.each(ns, bench)
  end

  def table() do
    r = 100000
    :io.format("# Benchmark of tuple operations, (~w runs), times in ms~n#~n", [r])
    :io.format("# n * 1000 is size of tuple~n", [])
    :io.format("#~7s ~8s ~8s~n", ["n", "lookup", "modify"])
    ns = [1000,2000,4000,8000,16000,32000]

    bench = fn(n) ->
      table = init_table(n)
      ops = init_ops(r,n)
      tl  = time(fn() ->  Enum.each(ops, fn(s) -> Table.lookup(table, s)      end) end)
      tm  = time(fn() ->  Enum.each(ops, fn(s) -> Table.modify(table, s, :foo) end) end)
      :io.format("~8w ~8w ~8w ~n", [div(n,1000), tl, tm])
    end
    Enum.each(ns, bench)
  end

  def map() do
    r = 100000
    :io.format("# Benchmark of map operations, (~w runs), times in ms~n#~n", [r])
    :io.format("# n * 1000 is size of map~n", [])
    :io.format("#~7s ~8s ~8s~n", ["n", "lookup", "modify"])
    ns = [1000,2000,4000,8000,16000,32000,64000,128000,256000,512000,1024000]

    bench = fn(n) ->
      map = init_map(n)
      ops = init_ops(r,n)
      tl  = time(fn() ->  Enum.each(ops, fn(s) -> Map.get(map, s, nil)  end) end)
      tm  = time(fn() ->  Enum.each(ops, fn(s) -> Map.replace!(map, s, :foo) end) end)
      :io.format("~8w ~8w ~8w ~n", [div(n,1000), tl, tm])
    end
    Enum.each(ns, bench)
  end

  def comp() do
    r = 100000
    :io.format("# Benchmark of tree vs tuple, modify operations, (~w runs), times in ms~n#~n", [r])
    :io.format("# n  is is size of tree/tuple~n", [])
    :io.format("#~7s ~8s ~8s ~n", ["n", "tree", "tuple"])
    ns = [100,200,400,800,1600,3200]
    bench = fn(n) ->
       ## create a list of n random number
       table = init_table(n)
       tree = init_tree(n)		    
       ops = init_ops(r,n)
       tr  = time(fn() -> Enum.each(ops, fn(s) -> Tree.modify(tree, s, :foo) end) end)
       tu  = time(fn() ->  Enum.each(ops, fn(s) -> Table.modify(table, s, :foo) end) end)
       :io.format("~8w ~8w ~8w ~n", [n, tr, tu])
    end
    Enum.each(ns, bench)
  end


  def init_table(n) do
    Table.new(Enum.map(:lists.seq(1,n), fn(_) -> :na end))
  end
    
  def init_tree(n) do
    seq = random(n,n)
    empty = Tree.empty()
    List.foldl(seq, empty, fn(s, t) -> Tree.insert(t, s, :na) end)
  end

  def init_map(n) do
    Map.new(Enum.map(:lists.seq(0,n-1), fn(n) -> {n, :na} end))
  end
  

  def init_ops(r,n) do
    random(r,n)
  end


  
  ## a list of random number 0..(n-1)
  def random(0,_) do  [] end
  def random(r,n) do [:rand.uniform(n)-1 | random(r-1, n)] end

  def time(f) do
    t1 = System.monotonic_time(:millisecond)
    f.()
    t2 = System.monotonic_time(:millisecond)
    t2 - t1
  end

end
