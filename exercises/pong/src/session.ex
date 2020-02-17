defmodule Session do

  def start(name, server) do
    spawn_link(fn() -> init(name, server) end)
  end


  def init(name, server) do
    receive do
    {:ws,  ws, :open} ->
	:io.format("pong: player (~w)  connected\n", [self()])
	send(server, {:ready, name})
	session(name, server, ws)
	:io.format("pong: session stopped~n")
    end
  end

  def session(name, server, ws) do
    receive do 
      {:ws, ws, {:msg, <<?D, _::binary>>}} ->
	send(server, {name, :down})
	session(name, server, ws)
      {:ws, ws, {:msg, <<?U, _::binary>>}} ->
	send(server, {name, :up})
	session(name, server, ws)

      {:player1, :up} ->
	send(ws, {:frw, <<?P,?U>>})
	session(name, server, ws)

      {:player1, :down} ->
	send(ws, {:frw, <<?P,?D>>})		
	session(name, server, ws)
	
      {:player1, :score, score} ->
	send(ws, {:frw, <<?P,?S, score>>})
	session(name, server, ws)
	
      {:player2, :up} ->
	send(ws, {:frw, <<?O,?U>>})
	session(name, server, ws)
	
      {:player2, :down} ->
	send(ws, {:frw, <<?O,?D>>})		
	session(name, server, ws)

      {:player2, :score, score} ->
	send(ws, {:frw, <<?O,?S, score>>})
	session(name, server, ws)
	
      {:ball, bx, by} ->
	send(ws, {:frw, <<?B,bx::16,by::16>>})
	session(name, server, ws)
	
      {:frw, msg} ->
	## arbitary messages 
	send(ws, {:frw, msg})
	session(name, server, ws)

      :stop ->
	:ok
    end
    
  end



end
