defmodule Ranges do

  def empty() do {:range, []} end
  
  def range(from, to) do {:range, [{from, to}]} end

  ## -- set operations
  
  def intersection({:range, a}, {:range, b}) do
    {:range, intersect(a, b)}
  end

  def union({:range, a}, {:range, b}) do
    {:range, unite(a, b)}
  end

  def difference({:range, a}, {:range, b}) do
    {:range, diff(a, b)}
  end

  ## -- arithmetic operations
  
  def add({:range, rng}, n) when is_number(n) do
    {:range, Enum.map(rng, fn({from,to}) -> {from+n,to+n} end)}
  end
  def add({:range, rnga}, {:range, rngb})  do
    List.foldl(rnga, [], fn({f1,t1},acc) ->
      List.foldl(rngb, acc, fn({f2, t2}, acc) ->
	unite([{f1+f2, t1+t2}], acc)
      end)
    end)
  end

  def mul({:range, rng}, n) when is_number(n) do
    cond do
      n > 0 ->
	{:range, Enum.map(rng, fn({from,to}) -> {from*n, to*n} end)}
      n == 0 ->
	{:range, [{0,0}]}
      true ->
	{:range, Enum.map(rng, fn({from,to}) -> {to*n, from*n} end)}
    end
  end
  
  def sub({:range, rng}, n) when is_number(n) do
    {:range, Enum.map(rng, fn({from,to}) -> {from-n,to-n} end)}
  end
  def sub(a, b) do
    add(a, mul(b,-1))
  end
    

  ## -- properties
  
  def min({:range, [{f,_}|_]}) do f end

  def max({:range, rng}) do
    [{_,t}] = Enum.take(rng, -1)
    t
  end

  def segments({:range, seg}) do length(seg) end  

  ##  -- the internals
  
  def diff(a, []) do a end
  def diff([], _) do [] end
  def diff([{f1, t1} | rest1]=range1, [{f2, t2} | rest2]=range2 ) do
    cond do
      t1 < f2  ->  [{f1,t1} | diff(rest1, range2)]
      t2 < f1  ->  diff(range1, rest2)
      f1 < f2  ->  [{f1,f2+1} | diff([{f2,t1}|rest1], [{f2,t2}| rest2])]
      t1 < t2  ->  diff(rest1, [{t1+1,t2} | rest2])	    
      t1 == t2 ->  diff(rest1, rest2)
      t2 < t1  ->  diff([{t2+1,t1}|rest1],  rest2)
    end
  end
  

  def intersect([], _) do [] end
  def intersect(_, []) do [] end  
  def intersect([{f1, t1} | rest1]=range1, [{f2, t2} | rest2]=range2 ) do
    cond do
      t1 < f2  ->  intersect(rest1, range2)
      t2 < f1  ->  intersect(range1, rest2)
      t1 < t2  ->  [{max(f1,f2), t1} | intersect(rest1,  [{t1+1,t2} | rest2])]
      t1 == t2 ->  [{max(f1,f2), t1} | intersect(rest1,  rest2)]
      t2 < t1  ->  [{max(f1,f2), t2} | intersect([{t2+1,t1}|rest1],  rest2)]
    end
  end    


  def unite([],b) do b end
  def unite(a,[]) do a end  
  def unite([{f1, t1} | rest1]=range1, [{f2, t2} | rest2]=range2 ) do
    cond do
      (t1+1) < f2 ->  [{f1,t1} | unite(rest1, range2)]
      (t2+1) < f1 ->  [{f2,t2} | unite(range1, rest2)]
      t1 == f2    ->  unite(rest1, [{f1,t2}| rest2])
      t2 == f1    ->  unite([{f2,t1}|rest1],  rest2)
      t1 <= t2    ->  unite(rest1,  [{min(f1,f2), t2}|rest2])
      t2 < t1     ->  unite([{min(f1,f2), t1}|rest1],  rest2)
    end
  end    
  
  

end
