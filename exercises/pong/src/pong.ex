defmodule Pong do

  @port   8080
  @tick    100
  @serve  4000
  
  def start() do
    spawn(fn() -> init() end)
  end

  defp init() do

    pong = self()
    ses1 = Session.start(:player1, pong)
    #ses2 = Session.start(:player2, pong)

    ses2 = Ping.start(:player2, pong)
    
    websocket = WebSocket.start(@port, [ses1])

    receive do
      {:ready, name} ->
	:io.format("~w ready~n", [name])
	receive do
	  {:ready, name} ->
	    :io.format("~w ready~n", [name])
	    :timer.send_after(@serve, self(), {:serve, :player1})
	    player1 = Game.player1(:player1)
	    player2 = Game.player2(:player2)
	    pong(player1, player2, :na, {0,0}, [ses1, ses2])
	    :io.format("pong: game stopped~n")	    
	  :stop ->
	    :ok
	end
      :stop ->
	:ok
    end
    :io.format("pong: server stopped~n")
    send(ses1, :stop)
    send(ses2, :stop)    
    send(websocket, :stop)
  end

  def broadcast(pids, msg) do
    Enum.each(pids, fn(pid) -> send(pid,  msg) end)
  end

  def pong(player1, player2, ball, score, sessions) do
    receive  do

      {:player1, :down} ->
	case Game.down(player1) do
	  {:ok, player1} ->
	    broadcast(sessions, {:player1, :down})
	    pong(player1, player2, ball, score, sessions)
	  :no ->
	    pong(player1, player2, ball, score, sessions)
	end

      {:player1, :up}->
	case Game.up(player1) do
	  {:ok, player1} ->
	    broadcast(sessions, {:player1, :up})
	    pong(player1, player2, ball, score, sessions)
	  :no ->
	    pong(player1, player2, ball, score, sessions)
	end

      {:player2, :down} ->
	case Game.down(player2) do
	  {:ok, player2} ->	
	    broadcast(sessions, {:player2, :down})
	  pong(player1, player2, ball, score, sessions)
	  :no ->
	    pong(player1, player2, ball, score, sessions)
	end


      {:player2, :up} ->
	case Game.up(player2) do
	  {:ok, player2} ->
	    broadcast(sessions, {:player2, :up})
	    pong(player1, player2, ball, score, sessions)
	  :no ->
	    pong(player1, player2, ball, score, sessions)
	end

      {:serve, :player1} ->
	{pos, ball}  = Game.serve(player1)
	tick = @tick
	:timer.send_after(tick, self(), {:tick, tick})
	broadcast(sessions, {:ball, pos})	
	pong(player1, player2, ball, score, sessions)

      {:serve, :player2} ->
	{pos, ball}  = Game.serve(player2)
	tick = @tick
	:timer.send_after(tick, self(), {:tick, tick})
	broadcast(sessions, {:ball, pos})	
	pong(player1, player2, ball, score, sessions)

      {:tick, tick} ->
	
	case Game.move_ball(player1, player2, ball) do
	  {:moved, pos,  ball} ->
	    :timer.send_after(tick, self(), {:tick, tick})
	    broadcast(sessions,  {:ball, pos})
	    pong(player1, player2,  ball, score, sessions)

	  {:bounce, pos, ball} ->
	    tick = if (tick > 10) do tick - 1 else tick end
	    :timer.send_after(tick, self(), {:tick, tick})
	    broadcast(sessions,  {:ball, pos})
	    pong(player1, player2,  ball, score, sessions)

	  {:score, :player1} ->
	    {s1,s2} = score
	    s1 = s1 + 1
	    broadcast(sessions,  {:ball, :hide})
	    broadcast(sessions,  {:player1, :score, s1})
	    :timer.send_after(@serve, self(), {:serve, :player1})	    
	    pong(player1, player2, :na, {s1, s2}, sessions)

	  {:score, :player2} ->
	    {s1,s2} = score
	    s2 = s2+1
	    broadcast(sessions,  {:ball, :hide})
	    broadcast(sessions,  {:player2, :score, s2})
	    :timer.send_after(@serve, self(), {:serve, :player2})	    
	    pong(player1, player2, :na, {s1,s2}, sessions)
	    
	end

      {:player1, :close} ->
	:io.format("pong: player 1 closed connection\n" )
	:ok

      {:player2, :close} ->
	:io.format("pong: player 2 closed connection\n" )
	:ok

      {:frw, msg} ->
	## if we want to send anything else to the sessions
	:io.format("msg to players: ~w\n", [msg])
	broadcast(sessions, {:frw, msg})
	pong(player1, player2, ball, score, sessions)


      :update ->
	Pong.pong(player1, player2, ball, score, sessions)
	
      :stop ->
	:ok

      strange ->
	:io.format("pong: received strange message ~w~n", [strange])
	pong(player1, player2, ball, score, sessions)	
    end
  end

	
    


end
