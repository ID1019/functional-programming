defmodule Ping do

  @height 600
  @paddle_height  100
  @move 5
  
  
  def start(name, server) do
    spawn_link(fn() -> init(name, server) end)
  end

  def init(name, server) do
    :io.format("~w: robot started~n", [name])
    send(server, {:ready, name})
    ping(name, server, div(@height,2))
    :io.format("~w: robot stoped~n", [name])
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

      {_, :down} ->
	ping(name, server, pos)

      {_, :up} ->
	ping(name, server, pos)

      {^name, :score, _} ->
	:io.format("GOOOOOOAAAAAALLLLL!!!!\n")
	ping(name, server, pos)

      {_, :score, _} ->
	:io.format("Offside! Hands! .... VAAAAAR!!!")
	ping(name, server, pos)
	
      :stop ->
	:ok

      strange ->
	:io.format("~w: strange message ~w~n", [name, strange])
	ping(name, server, pos)	
    end
  end    
    
  
end
