defmodule Day16 do

  def dynamic(t) do
    start = :AA
    ## {closed, graph} = Cave.sample(start)
    {closed, graph} = Cave.input(start) 
    {max, _} = search(start, t, closed, [], 0, graph, Map.new())
    max
  end

  def check(valve, t, closed, open, rate, graph, mem) do
    case mem[{valve, t, open}] do
      nil ->
	{max, mem} = search(valve, t, closed, open, rate, graph, mem)
	mem = Map.put(mem, {valve, t, open}, max)
	{max, mem}
      max ->
	{max, mem}
    end
  end


  def search(_, 0, _, _, _, _, _, mem) do
    {0, mem}
  end  
  def search(_, t, [], _, rate, _, mem) do
    total = rate * t
    ## We actually learned something but why add it to the memory,
    ## it will not save us any work. 
    {total, mem}
  end  
  def search(valve, t, closed, open, rate, graph, mem) do

    {rt, tunnels} = graph[valve]

    ## mx will be the best option so far
    
    ## One option is to stay put and do nothing.
    mx = rate*t

    ## If we have a valve to open that might be a better option
    {mx, mem} = if (rt > 0 and Enum.member?(closed, valve)) do
      removed = List.delete(closed, valve)
      {ox, mem} = check(valve, t-1, removed, insert(open, valve), rate+rt, graph, mem)
      ox = ox + rate
      {max(ox,mx), mem}
    else
      {mx, mem}
    end

    ## Try moving to each of the tunnels.
    
    Enum.reduce(tunnels, {mx, mem}, 
      fn({nxt, d}, {mx, mem}) ->
        if (d < t) do
	  {ox, mem} = check(nxt, t-d, closed, open, rate, graph, mem)
	  ox = ox + (rate *d)
	  {max(ox,mx), mem}
	else
	  {mx, mem}
	end
      end)
  end

  ## Keep the list ordered to work better as key.
  
  def insert([], valve) do [valve] end
  def insert([v|rest], valve) when v < valve do  [v|insert(rest, valve)] end
  def insert(open, valve) do  [valve|open] end


end
