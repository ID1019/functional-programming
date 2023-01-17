defmodule Day16a do

  def input() do
    ##  |>
    sample() |>
      parse()
  end

  def task() do
    task(30)
  end
  
  def task(t) do
    start = :AA

    #rows = File.stream!("day16.csv")
    rows = sample()
    map = Map.new(parse(rows))

    closed = Enum.map((Enum.filter(map, fn({_,{rate,_}}) -> rate != 0 end)), fn({valve, _}) -> valve end)

    {max, path} = search(start, t, closed, [], 0, map, [])
    {max, Enum.reverse(path)}
  end

  ##
  ##  search(valve, t, closed, open, rate, map, path)
  ##
  ##  We're standing at valve and have t min left.  Some valves are
  ##  still closed, while some are open. We know the current rate and
  ##  need to calculate the maximum obtainable flow.
  ##
  ##  We return a tuple: the total flow and the path i.e. the sequnce
  ##  of valves to open.
  
  def search(_valve, 0, _valves, _open, _rate, _map, path) do
    {0, path}
  end

  def search(_valve, t, [], _open, rate, _map, path) do
    ## all valves are open
    {rate*t, path}
  end  

  def search(valve, t, closed, open, rate, map, path) do

    {rt, tunnels} = map[valve]
    
    {mx, pathx} = if Enum.member?(closed, valve) do
      ## open the valve is one option
      removed = List.delete(closed, valve)
      added = insert(open, valve)
      {mx, pathx} = search(valve, t-1, removed, added, rate+rt, map, [valve|path])
      mx = mx + rate
      {mx, pathx}
    else
      ## if we the valve is open we could just stay
      {rate*t, path}
    end

    Enum.reduce(tunnels, {mx, pathx}, 
      fn(nxt, {mx, pathx}) ->
        ## moving to nxt 
	{my, pathy} = search(nxt, t-1, closed, open, rate, map, path)
	my = my + rate
	if (my > mx) do
	  ## moving to nxt was better
	  {my, pathy}
	else
	  {mx, pathx}
	end   
      end)
  end

  ## let's keep the open valves in order, could be an advantage
  
  def insert([], valve) do [valve] end
  def insert([v|rest], valve) when v < valve do  [v|insert(rest, valve)] end
  def insert(open, valve) do  [valve|open] end


  ## turning strings
  ##
  ##   "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE" 
  ##
  ## to tuples 
  ##
  ##  {:DD, {20, [:CC, :AA, :EE]}
  ##

  def parse(input) do
    Enum.map(input, fn(row) ->
      [valve, rate, valves] = String.split(String.trim(row), ["=", ";"])
      [_Valve, valve | _has_flow_rate ] = String.split(valve, [" "])
      valve = String.to_atom(valve)
      {rate,_} = Integer.parse(rate)
      [_, _tunnels,_lead,_to,_valves| valves] = String.split(valves, [" "])
      valves = Enum.map(valves, fn(valve) -> String.to_atom(String.trim(valve,",")) end)
      {valve, {rate, valves}}
    end)
  end

  def sample() do
    ["Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
     "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
     "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
     "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
     "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
     "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
     "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
     "Valve HH has flow rate=22; tunnel leads to valve GG",
     "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
     "Valve JJ has flow rate=21; tunnel leads to valve II"]
  end
  

  
end

