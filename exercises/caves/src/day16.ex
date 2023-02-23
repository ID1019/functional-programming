defmodule Day16 do
  
  def task_a(t) do
    start = :AA
    {closed, graph} = Cave.sample(start)
    ##{closed, graph} = Cave.input(start) 
    elem(check(start, t, closed, graph, Map.new()),0)
  end

  def task_b(t) do
    start = :AA
    {[frst|closed], graph} = Cave.sample(start)
    ##{[frst|closed], graph} = Cave.input(start) 
    elem(split(start, t, closed, [frst], [], graph, Map.new()),0)
  end

  def split(start, t, [], you, elefant,  graph, mem) do
    :io.format("you: ~w\t\telefant: ~w", [you, elefant])
    {m1, mem} = search(start, t, you, graph, mem)
    {m2, mem} = search(start, t, elefant, graph, mem)    
    total = m1+m2
    :io.format("\t\ttotal: ~w\n", [m1+m2])
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
