defmodule Hinges do
  
  @moduledoc """

  The task is to maximize profit from producing hinges (gångjärn) and
  latches (haspar) given a set of requirements. We have a maximum amount
  of raw material and maximum time that we can use. the hinges and
  latches need different amount of resources and are sold at different
  prices. Find out how many hinges and latches that one should produce
  in order to maximize profit.
  """
  
  
  @doc """ 
  First compute the maximum profit if staring with a hinge and then do
  the same for a latch and take the maximum of the two answers.  
  """

    
  def search(m, t, {hm, ht, hp}=h, {lm, lt, lp}=l) when  (m >= hm) and (t >= ht) and (m >= lm) and (t >= lt) do
    {hi, li, pi} = search((m-hm), (t-ht), h, l)
    {hj, lj, pj} = search((m-lm), (t-lt), h, l)
    if (pi+hp) > (pj+lp) do
      {(hi+1), li, (pi+hp)};
    else
      {hj, (lj+1), (pj+lp)}
    end
  end
  def search(m, t, {hm, ht, hp}=h, l) when ((m >= hm) and (t >= ht))  do
    {hn, ln, p} = search((m-hm), (t-ht), h, l)
    {hn+1, ln, (p+hp)}
  end
  def search(m, t, h, {lm, lt, lp}=l) when ((m >= lm) and (t >= lt))  do
    {hn, ln, p} = search((m-lm), (t-lt), h, l)
    {hn, ln+1, p+lp}
  end
  def search(_, _, _, _) do
    {0,0,0}
  end
  

  @doc """
  The search/4 solution can be extended with a memory and then bring
  the compexity down to O(n^2) (or more precise O(m*t) ). 
  """

  def memory(m, t, h, l) do
    mem = Memory.new()
    {solution, _} = search(m, t, h, l, mem)
    solution
  end

  def check(m, t, h, l, mem) do
    case Memory.lookup({m,t}, mem) do
      nil ->
	{solution, mem} = search(m, t, h, l, mem)
	{solution, Memory.store({m,t}, solution, mem)}
      {_, solution} -> 
	{solution, mem}
    end
  end
  def search(m, t, {hm, ht, hp}=h, {lm, lt, lp}=l, mem) when (m >= hm) and (t >= ht) and (m >= lm) and (t >= lt) do
    {{hi, li, pi}, mem} = check((m-hm), (t-ht), h, l, mem)
    {{hj, lj, pj}, mem} = check((m-lm), (t-lt), h, l, mem)
    if (pi+hp) > (pj+lp) do
      {{(hi+1), li, (pi+hp)}, mem}
    else
      {{hj, (lj+1), (pj+lp)}, mem}
    end
  end
  def search(m, t, {hm, ht, hp}=h, l, mem) when ((m >= hm) and (t >= ht)) do
    {{hn, ln, p}, mem} = check((m-hm), (t-ht), h, l, mem)
    {{hn+1, ln, (p+hp)}, mem}
  end
  def search(m, t, h, {lm, lt, lp}=l, mem) when ((m >= lm) and (t >= lt)) do
    {{hn, ln, p}, mem} = check((m-lm), (t-lt), h, l, mem)
    {{hn, ln+1, p+lp}, mem}
  end
  def search(_, _, _,  _, mem)  do
    {{0,0,0}, mem}
  end

  @doc """
  In the ordered/4 version we only check sequences that start with
  hinges and continues with latches. This brings the computation time
  down to O(n^2) same as the easy one.

  Note that for this problem there is no point in trying to save
  answers since the computation does not recompute any points
  (unless the hinges and latches have the same parameters). If we
  had three items the computations would make duplicates and it
  could be profitable to store computetd values.
  """

  def ordered(m, t, {hm, ht, _}, l) when (m < hm) or (t < ht) do
    {n, p} = then_latches(m, t, l)
    {0, n, p}
  end
  def ordered(m, t, {hm, ht, hp}=h, l) do
    {hi, li, pi} = ordered((m-hm), (t-ht), h, l)
    latches = then_latches(m, t, l)
    {lj, pj} = latches
    if (pi+hp) > pj do
      {(hi+1), li, (pi+hp)}
    else
      {0, lj, pj}
    end
  end

  def then_latches(m, t, {lm, lt, _}) when (m < lm) or (t < lt) do
    {0, 0}
  end
  def then_latches(m, t, {lm, lt, lp}=l) do
    {n, p} = then_latches((m-lm), (t-lt), l)
    {n+1, p+lp}
  end




  @doc """

  The easy way is to simple generate all possible combinations of
  the number of hinges and latches and then check 1/ is it possible
  to make them given the resources and 2/ what the profit would
  be. the algorithm is O(n^2) so it's ok complexity wise. the
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

						     

    
	       
