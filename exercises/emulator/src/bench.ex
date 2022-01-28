defmodule Bench do

  ## n  -  number of operations
  ## r  -  read ratio i.e. of 0.2 then 20% of operations will be read operations
  ## k  -  the size of the memory k  adressed [0 ... (k-1)]

  def bench(n, r, k) do
    seq = Enum.map(1..n, fn _ -> rand(r, k) end)
    {dt, _} = :timer.tc(fn() -> dummy(k, seq) end)
    {lt, _} = :timer.tc(fn() -> list(k, seq) end)
    {mt, _} = :timer.tc(fn() -> map(k, seq) end)
    {tt, _} = :timer.tc(fn() -> tuple(k, seq) end) 
    ## dt - dummy
    ## lt - list
    ## mt - map (tree)
    ## tt - tuple
    {dt, lt, mt, tt}
  end

  def rand(r, k) do
    i = (:rand.uniform(k) -1 )
    
    if :rand.uniform(100)/100 < r do
      {:read, i}
    else
      {:write, i}
    end
  end


  def dummy(_, seq) do
     List.foldl(seq, :ok, fn (op, reg) -> dummy_op(op, reg) end)
  end

  def dummy_op(op, reg) do
    case op do
      {:read, _key} ->
	reg
      {:write, 0} ->
	reg	
      {:write, _key} ->
	reg
    end
  end

  def list(k, seq) do
     reg = Enum.map(0..(k-1), fn x -> {x, x} end)
     List.foldl(seq, reg, fn (op, reg) -> list_op(op, reg) end)
  end
  
  def list_op(op, reg) do
    case op do
      {:read, key} ->
	List.keyfind(reg, key, 0, {key, 0})
	reg
      {:write, 0} ->
	reg	
      {:write, key} ->
	List.keystore(reg, key, 0, {:key, 42})
    end
  end

  def map(k, seq) do
     reg = Map.new(0..(k-1), fn x -> {x, x} end)
     List.foldl(seq, reg, fn (op, reg) -> map_op(op, reg) end)
  end
  
  def map_op(op, reg) do
    case op do
      {:read, key} ->
        Map.get(reg, key, 0)
	reg
      {:write, 0} ->
	reg
      {:write, key} ->
	Map.put(reg, key, 42)
    end
  end

  

  def tuple(k, seq) do
     reg = List.to_tuple(Enum.to_list(0..(k-1)))
     List.foldl(seq, reg, fn (op, reg) -> tuple_op(op, reg) end)
  end
  
  def tuple_op(op, reg) do
    case op do
      {:read, key} ->
        _ = elem(reg, key)
	reg
      {:write, 0} ->
	reg
      {:write, i} ->
	put_elem(reg, i, 42)
    end
  end
  
  

end
