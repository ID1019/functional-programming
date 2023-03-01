defmodule Day16 do
  
  def task_a(t) do
    start = :AA
    ##{closed, graph} = Cave.sample(start)
    {closed, graph} = Cave.input(start) 
    elem(check(start, t, closed, graph, Map.new()),0)
  end

  def task_b(t) do
    start = :AA
    ##{[frst|closed], graph} = Cave.sample(start)
    {[frst|closed], graph} = Cave.input(start) 
    elem(split(start, t, closed, [frst], [], graph, Map.new()),0)
  end

  def split(start, t, [], you, elefant,  graph, mem) do
    s0 = map_size(mem)
    :io.format("you: ~2.w  elefant: ~2.w  size: ~8.w  ", [length(you), length(elefant), s0])
    {t1, {m1, mem}} = :timer.tc(fn() ->  search(start, t, you, graph, mem) end)
    {t2, {m2, mem}} = :timer.tc(fn() -> search(start, t, elefant, graph, mem) end)
    total = m1+m2
    s1 = map_size(mem)
    :io.format("added: ~6.w  time: ~6.w\n", [s1-s0, t1+t2])
    ##:io.put_chars('.')
    {total, mem}
  end
  def split(start, t, [valve|closed], you, elefant,  graph, mem) do
    {m1, mem} = split(start, t, closed, [valve|you], elefant, graph, mem)
    {m2, mem} = split(start, t, closed, you, [valve|elefant], graph, mem)    
    {max(m1,m2), mem}
  end

  ## The memory is organized so that it can be sused in several
  ## searches. The key is what is left, not how we got there, what
  ## valves are currently open or the rate at which the pressure is
  ## released.
  
  def check(valve, t, closed, graph, mem) do
    case mem[{valve, t, closed}] do
      nil ->
	{max, mem} = search(valve, t, closed, graph, mem)
	mem = Map.put(mem, {valve, t, closed}, max)
	{max, mem}
      max ->
	{max, mem}
    end
  end


  ## The graph is a completly connected graph continaing only the
  ## valves that should be opened and the starting node :AA. Since it
  ## is completely connecte we should always select a node with a
  ## closed valve, open the valve and continue the search. 
  
  def search(_, 0, _, _, _, mem) do
    {0, mem}
  end  
  def search(_, _, [], _, mem) do
    {0, mem}
  end  
  def search(valve, t, closed, graph, mem) do

    {rt, tunnels} = Cave.get(graph,valve)

    {t, closed} = if rt > 0  do
      {t - 1, List.delete(closed, valve)}
    else
      ## Only :AA will have a rate of 0 
      {t, closed}
    end

    {mx, mem} = Enum.reduce(tunnels, {0, mem}, 
      fn({nxt, d}, {mx, mem}) ->
        if ((d < (t-1)) and Enum.member?(closed, nxt)) do
     	  {ox, mem} = check(nxt, t-d,  closed, graph, mem)
     	  {max(ox,mx), mem}
     	else
     	  {mx, mem}
     	end
      end)
    {mx+(rt*t), mem}
  end


end
