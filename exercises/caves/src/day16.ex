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

  def search(_, 0, _, _, _, mem) do
    {0, mem}
  end  
  def search(_, _, [], _, mem) do
    {0, mem}
  end  
  def search(valve, t, closed, graph, mem) do

    {rt, tunnels} = Cave.get(graph,valve)

    ## mx will be the best option so far
    
    ## If we have a valve to open, that might be an idea
    {mx, mem} = if (rt > 0 and Enum.member?(closed, valve)) do
      removed = List.delete(closed, valve)
      {ox, mem} = check(valve, t-1, removed, graph, mem)
      ox = ox + (rt * (t-1))
      {ox, mem}
    else
      {0, mem}
    end

    ## Try moving to each of the tunnels.
    
    Enum.reduce(tunnels, {mx, mem}, 
      fn({nxt, d}, {mx, mem}) ->
        if (d < t) do
	  {ox, mem} = check(nxt, t-d, closed, graph, mem)
	  {max(ox,mx), mem}
	else
	  {mx, mem}
	end
      end)
  end


end
