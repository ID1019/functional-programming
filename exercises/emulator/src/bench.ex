defmodule Bench do


  def bench(n, r, k) do
    seq = for _ <- 1..n, do: rand(r, k)
    reg = List.to_tuple(Enum.to_list(0..k-1))
    {dt, _} = :timer.tc(fn() -> List.foldl(seq, [], fn(x,a) -> dummy(a, x) end) end)
    {lt, _} = :timer.tc(fn() -> List.foldl(seq, [], fn(x,a) -> list(a, x) end) end)
    {mt, _} = :timer.tc(fn() -> List.foldl(seq, %{}, fn(x,a) -> map(a, x) end) end)
    {tt, _} = :timer.tc(fn() -> List.foldl(seq, reg, fn(x,a) -> tuple(a, x) end) end)
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

  
  def dummy(reg, op) do
    case op do
      {:read, _key} ->
	reg
      {:write, 0} ->
	reg	
      {:write, _key} ->
	reg
    end
  end

  def list(reg, op) do
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

  def map(reg, op) do
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
  
  def tuple(reg, op) do
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
