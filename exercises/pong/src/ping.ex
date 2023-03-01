defmodule Ping do

  @height 600
  @paddle_height  100
  @move 5
  
  
  def start(name, server) do
    spawn_link(fn() -> init(name, server) end)
  end

  def init(name, server) do
    :io.format("ping: robot ~w started~n", [name])
    send(server, {:ready, name})
    ping(name, server, div(@height,2))
    :io.format("ping: robot ~w stoped~n", [name])
  end
  

  def ping(name, server, pos) do
    receive do
      {:ball, {_, by}} ->
	dist = pos - by
	cond  do
	  abs(dist) < div(@paddle_height,2) ->
	    # safe 
	    :remain
	  dist > 0 ->
	    # we need a burst of moves 
	    send(server, {name, :up})
	    send(server, {name, :up})
	    send(server, {name, :up})
	    send(server, {name, :up})	    
	    send(server, {name, :up})
	  true ->
	    # we need a burst of moves 
	    send(server, {name, :down})
	    send(server, {name, :down})
	    send(server, {name, :down})
	    send(server, {name, :down})
	    send(server, {name, :down})	    	    
	end
	ping(name, server, pos)

      {^name, :down} ->
	ping(name, server, pos+@move)

      {^name, :up} ->
	ping(name, server, pos-@move)

      :stop ->
	:ok

      _ ->
	ping(name, server, pos)	
    end
  end    
    
  
end
