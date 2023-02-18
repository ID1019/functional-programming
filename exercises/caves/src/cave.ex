defmodule Cave do

  def input(start) do
    graph = File.stream!("day16.csv") |>
      parse() |> reduce(start)
    valves = valves(graph)
    {valves, graph}
  end

  def sample(start) do
    graph = sample() |>
      parse() |> reduce(start)
    valves = valves(graph)
    {valves, graph}
  end

  def get(graph, valve) do
    graph[valve]
  end
  
  def valves(graph) do
    Enum.filter(graph, fn({_,{rt,_}}) -> rt > 0 end) |>
      Keyword.keys()
  end


  ## Reduce the graph so that it only contains tunnels with valves.
  
  def reduce(input, start) do

    ## divide tunnels into:
    ##   valves: that have rate above 0  and
    ##   conn: that are tunnels with flow equal to 0 i.e. connecting tunnels

    {valves, conn} = Enum.split_with(input,  fn({_, {rate,_}}) -> (rate != 0)  end)

    ## Add the staring position to the valves if it is not already there.

    valves = if (valves[start] == nil) do
      [{start, conn[start]} | valves]
    else
      valves
    end

    ## First extend the connecting tunnels so that they all connect
    ## only to tunnels with valves i.e. tunnels that are not in the
    ## set.

    conn = extend(conn)

    ## Then extend all tunnels with valves (and the starting tunnel),
    ## replacing tunnels that are only serving as connections. The
    ## result will be a graph with nodes only between tunnels with
    ## valves. The starting tunnel is always present but if it does
    ## not have a valve no other tunnel will lead to it. 
    
    extend(conn, valves)
  end

  def extend(conn) do
    Enum.reduce(Keyword.keys(conn), conn, fn(v0, conn) ->
      {_, t0} = Keyword.fetch!(conn, v0)
      Enum.map(conn, fn({v1, {r1,t1}}) ->
	{v1, {r1, extend(v1, t1, v0, t0)}}
      end)	
    end)
  end
  

  def extend(conn, valves) do
    Enum.reduce(conn, valves, fn({v0, {_, t0}}, valves) ->
      Enum.map(valves, fn({v1, {r1,t1}}) ->
	{v1, {r1, extend(v1, t1, v0, t0)}}
      end)
    end)
  end
  
  def extend(v1, t1, v0, t0) do
      case List.keyfind(t1, v0, 0) do
	{v0, k0} ->
	  removed = List.keydelete(t1, v0, 0)
	  Enum.reduce(t0, removed, fn({v2,k2}, acc) ->
	    if (v2 != v1 ) do
	      case List.keyfind(removed, v2, 0) do
		{v2,k3} ->
		  [{v2, min(k3, k2+k0)}|List.keydelete(acc, v2,0)]
		nil ->
		  [{v2, k2+k0}|acc]
	      end
	    else
	      acc
	    end
	  end)
	nil ->
	  t1
      end
  end
  

  def parse(input) do
    Enum.map(input, fn(row) ->
      [valve, rate, valves] = String.split(String.trim(row), ["=", ";"])
      [_, valve | _ ] = String.split(valve, [" "])
      valve = String.to_atom(valve)
      {rate,_} = Integer.parse(rate)
      [_,_,_,_, _| valves] = String.split(valves, [" "])
      valves = Enum.map(valves, fn(v) -> {String.to_atom(String.trim(v,",")), 1} end)
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
