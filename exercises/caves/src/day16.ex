defmodule Day16 do

  def dynamic(t) do
    start = :AA
    graph = Cave.sample(start)
    ##graph = Cave.input(start)
    valves = Enum.map(graph, fn({valve,_}) -> valve end)
    {max, _} = search(start, t, valves, [], 0, graph, Map.new())
     max
  end

  def check(valve, t, valves, open, rate, graph, mem) do
    case mem[{valve, t, open}] do
      nil ->
	## :io.format("searching ~w ~w ~w\n", [valve, t, open])
	{max, mem} = search(valve, t, valves, open, rate, graph, mem)
	mem = Map.put(mem, {valve, t, open}, max)
	{max, mem}
      max ->
	{max, mem}
    end
  end


  def search(_, 0, _, _, _, _, _, mem) do
    ## :io.format("time-out: \n")
    {0, mem}
  end  
  def search(valve, t, [], open, rate, _, mem) do
    total = rate * t
    ## :io.format("all open: ~w\n", [total])
    mem = Map.put(mem, {valve, t, open}, total)
    {total, mem}
  end  
  def search(valve, t, valves, open, rate, graph, mem) do

    {rt, tunnels} = graph[valve]
    
    {mx, mem} = if Enum.member?(valves, valve) do
      ## open the valve is one option
      removed = List.delete(valves, valve)
      {mx, mem} = check(valve, t-1, removed, insert(open, valve), rate+rt, graph, mem)
      mx = mx + rate
      {mx, mem}
    else
      ## if we can not open the valve we could just stay
      {rate*t, mem}
    end

    Enum.reduce(tunnels, {mx, mem}, 
      fn({nxt, d}, {mx, mem}) ->
        if (d < t) do
          ## moving to nxt 
	  {my, mem} = check(nxt, t-d, valves, open, rate, graph, mem)
	  my = my + (rate *d)
	  if (my > mx) do
	    ## moving to nxt was better
	    {my, mem}
	  else
	    {mx, mem}
	  end
	else
	  {mx, mem}
	end
      end)
  end

  def insert([], valve) do [valve] end
  def insert([v|rest], valve) when v < valve do  [v|insert(rest, valve)] end
  def insert(open, valve) do  [valve|open] end


end
