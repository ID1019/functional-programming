defmodule Day16b do

  def input() do
    ## File.stream!("day16.csv") |>
    sample() |>
      parse() |>
    Enum.to_list()
  end

  def task_a(t) do
    start = :AA
    map = input()
    map = Map.new(map)
    valves = Enum.map((Enum.filter(map, fn({_,{rate,_}}) -> rate != 0 end)), fn({valve, _}) -> valve end)
    {max, _, path} = dynamic(start, t, valves, [], 0, map, Map.new(), [])
    {max, Enum.reverse(path)}
  end

  def dynamic(valve, t, [], open, rate, _, mem, path) do
    total = rate * t
    mem = Map.put(mem, {valve, t, open}, {total,path})
    {total, mem, path}
  end
  def dynamic(valve, t, valves, open, rate, map, mem, path) do
    case mem[{valve, t, open}] do
      nil ->
	{max, mem, path} = search(valve, t, valves, open, rate, map, mem, path)
	mem = Map.put(mem, {valve, t, open}, {max,path})
	{max, mem, path}
      {max, path} ->
	{max, mem, path}
    end
  end


  ##  if t > 0 , valves =/= [] i.e. we still have a choice
  ##      - open valve if possible
  ##      - move through any of tunnels
  ##
  def search(valve, 0, _valves, _open, _rate, _map, _mem, path) do
    {0, mem, path}
  end

  def search(valve, t, valves, open, rate, map, mem, path) do

    {rt, tunnels} = map[valve]
    
    {mx, mem, pathx} = if Enum.member?(valves, valve) and (rt != 0) do
      ## open the valve is one option
      removed = List.delete(valves, valve)
      {mx, mem, pathx} = dynamic(valve, t-1, removed, insert(open, valve), rate+rt, map, mem, [valve|path])
      mx = mx + rate
      {mx, mem, pathx}
    else
      ## if we can not open the valve we could just stay
      {rate*t, mem, path}
    end

    Enum.reduce(tunnels, {mx, mem, pathx}, 
      fn(nxt, {mx, mem, pathx}) ->
        ## moving to nxt 
	{my, mem, pathy} = dynamic(nxt, t-1, valves, open, rate, map, mem, path)
	my = my + rate
	if (my > mx) do
	  ## moving to nxt was better
	  {my, mem, pathy}
	else
	  {mx, mem, pathx}
	end
      end)
  end

  ## open valves in order 
  
  def insert([], valve) do [valve] end
  def insert([v|rest], valve) when v < valve do  [v|insert(rest, valve)] end
  def insert(open, valve) do  [valve|open] end
  

  def parse(input) do
    Stream.map(input, fn(row) ->
      [valve, rate, valves] = String.split(String.trim(row), ["=", ";"])
      [_, valve | _ ] = String.split(valve, [" "])
      valve = String.to_atom(valve)
      {rate,_} = Integer.parse(rate)
      [_,_,_,_, _| valves] = String.split(valves, [" "])
      valves = Enum.map(valves, &String.to_atom(String.trim(&1,",")))
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

