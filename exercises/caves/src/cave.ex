defmodule Cave do

  def input(start) do
    File.stream!("day16.csv") |>
      parse() |>
      graph(start)
  end

  def sample(start) do
    sample() |>
      parse() |>
      graph(start)
  end

  ## build a graph given the specification
  ## input on the form {valve, {rate, [{v1,1}, {v2,1} .. ]}}
  
  def graph(input, start) do

    ## divide tunnels into:
    ##   valves that have rate above 0 (or is the starting position) and
    ##   conn that are tunnels with flow equal to 0

    {valves, conn} = Enum.split_with(input,  fn({valve, {rate,_}}) -> (rate != 0) or (valve == start) end)

    ## Create a map where the valves are directly connected to other
    ## valves i.e. tunnels are removes.

    ## First extend the connecting tunnels ...

    conn = extend(conn, conn)

    ## Then extend all valves, replacing tunnels that are only serving as connections.
    
    valves = extend(conn, valves)

    ## The resulting graph will have tunnels with valves directly
    ## connected to other tunnels with valves (plus the starting
    ## position, even if it has no valve)
    
    Map.new(valves)

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
	{v0, _k0} ->
	  removed = List.keydelete(t1, v0, 0)
	  Enum.reduce(t0, removed, fn({v2,k2}, acc) ->
	    if (v2 != v1 ) do
	      case List.keyfind(removed, v2, 0) do
		{v2,k3} ->
		  [{v2, min(k3, k2+1)}|List.keydelete(acc, v2,0)]
		nil ->
		  [{v2, k2+1}|acc]
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
