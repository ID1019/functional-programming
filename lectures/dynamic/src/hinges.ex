defmodule Hinges do
  
  @moduledoc """

  The task is to maximize profit from producing hinges (gångjärn) and
  latches (haspar) given a set of requirements. We have a maximum amount
  of raw material and maximum time that we can use. the hinges and
  latches need different amount of resources and are sold at different
  prices. Find out how many hinges and latches that one should produce
  in order to maximize profit.
  """
  
  def search(m, t, h, l) do
    memory(m, t, h, l, None)
  end
  
  def memory(m, t, h, l) do
    memory(m, t, h, l, Memory)
  end

  def map(m, t, h, l) do
    memory(m, t, h, l, Better)
  end  
    
  
  def memory(m, t, h, l, module) do
    mem = module.new()
    {solution, _} = search(m, t, h, l, mem, module)
    solution
  end

  def check(m, t, h, l, mem, module) do
    case module.lookup({m,t}, mem) do
      nil ->
	{solution, mem} = search(m, t, h, l, mem, module)
	{solution, module.store({m,t}, solution, mem)}
      solution -> 
	{solution, mem}
    end
  end

  def search(m, t, {hm, ht, hp}=h, {lm, lt, lp}=l, mem, module) when (m >= hm) and (t >= ht) and (m >= lm) and (t >= lt) do
    {{hi, li, pi}, mem} = check((m-hm), (t-ht), h, l, mem, module)
    {{hj, lj, pj}, mem} = check((m-lm), (t-lt), h, l, mem, module)
    if (pi+hp) > (pj+lp) do
      {{(hi+1), li, (pi+hp)}, mem}
    else
      {{hj, (lj+1), (pj+lp)}, mem}
    end
  end
  def search(m, t, {hm, ht, hp}=h, l, mem, module) when ((m >= hm) and (t >= ht)) do
    {{hn, ln, p}, mem} = check((m-hm), (t-ht), h, l, mem, module)
    {{hn+1, ln, (p+hp)}, mem}
  end
  def search(m, t, h, {lm, lt, lp}=l, mem, module) when ((m >= lm) and (t >= lt)) do
    {{hn, ln, p}, mem} = check((m-lm), (t-lt), h, l, mem, module)
    {{hn, ln+1, p+lp}, mem}
  end
  def search(_, _, _,  _, mem, _)  do
    {{0,0,0}, mem}
  end


  @doc """

  The easy way is to simple generate all possible combinations of
  the number of hinges and latches and then check 1/ that it is possible
  to make them given the resources and 2/ what the profit would
  be. the algorithm is O(n^2) so it's ok complexity wise. The
  solution relies on the fact that we can compute a finite set of
  possible solutions and then iterate over these.
  """

  def easy(m, t, {hm, ht, hp}, {lm, lt, lp}) do
    maxh = max(trunc(m/hm), trunc(t/ht))
    maxl = max(trunc(m/lm), trunc(t/lt))
    all =  for h <- 1..maxh, l <- 1..maxl, do: {h,l}
    select = fn ({h,l}) -> ((h*hm + l*lm) <= m) and ((h*ht + l*lt) <= t) end
    possible = Enum.filter(all, select)
    profit = Enum.map(possible, fn ({h, l}) -> {h, l, (h*hp + l*lp)} end)

    List.foldl(profit, {0,0,0}, fn ({h, l, p}, {_, _, pa}=acc) ->
      if p > pa do
	{h, l, p}
      else
	acc
      end
    end)
  end

end

						     

    
	       
