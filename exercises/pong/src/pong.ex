defmodule Pong do

  @tick    100
  @serve  4000
  
  def start(port) do
    spawn(fn() -> init(port) end)
  end

  defp init(port) do

    pong = self()
    ses1 = Session.start(:player1, pong)
    #ses2 = Session.start(:player2, pong)

    ses2 = Ping.start(:player2, pong)
    
    server = WebSocket.start(port, [ses1])

    receive do
      {:ready, name} ->
	:io.format("~w ready~n", [name])
	receive do
	  {:ready, name} ->
	    :io.format("~w ready~n", [name])
	    :timer.send_after(@serve, self(), {:serve, :player1})
	    player1 = Game.player(:player1)
	    player2 = Game.player(:player2)
	    pong(player1, player2, :na, [ses1, ses2])
	  :stop ->
	    :ok
	end
      :stop ->
	:ok
    end
    :io.format("pong: server stopped~n")
    send(ses1, :stop)
    send(ses2, :stop)    
    send(server, :stop)
  end

  def broadcast(pids, msg) do
    Enum.each(pids, fn(pid) -> send(pid,  msg) end)
  end

  def pong(player1, player2, ball, sessions) do

    receive  do

      {:player1, :down} ->
	case Game.down(player1) do
	  {:ok, player1} ->
	    broadcast(sessions, {:player1, :down})
	    pong(player1, player2, ball, sessions)
	  :no ->
	    pong(player1, player2, ball, sessions)
	end

      {:player1, :up}->
	case Game.up(player1) do
	  {:ok, player1} ->
	    broadcast(sessions, {:player1, :up})
	    pong(player1, player2, ball, sessions)
	  :no ->
	    pong(player1, player2, ball, sessions)
	end

      {:player2, :down} ->
	case Game.down(player2) do
	  {:ok, player2} ->	
	    broadcast(sessions, {:player2, :down})
	  pong(player1, player2, ball, sessions)
	  :no ->
	    pong(player1, player2, ball, sessions)
	end


      {:player2, :up} ->
	case Game.up(player2) do
	  {:ok, player2} ->
	    broadcast(sessions, {:player2, :up})
	    pong(player1, player2, ball, sessions)
	  :no ->
	    pong(player1, player2, ball, sessions)
	end

      {:serve, turn} ->
	{pos, ball}  = Game.serve(turn, player1, player2)
	tick = @tick
	:timer.send_after(tick, self(), {:tick, tick})
	broadcast(sessions, {:ball, pos})	
	pong(player1, player2, ball, sessions)

      {:tick, tick} ->
	
	case Game.move_ball(player1, player2, ball) do
	  {:moved, pos,  ball} ->
	    :timer.send_after(tick, self(), {:tick, tick})
	    broadcast(sessions,  {:ball, pos})
	    pong(player1, player2,  ball, sessions)

	  {:bounce, pos, ball} ->
	    tick = if (tick > 10) do tick - 1 else tick end
	    :timer.send_after(tick, self(), {:tick, tick})
	    broadcast(sessions,  {:ball, pos})
	    pong(player1, player2,  ball, sessions)

	  {:score, :player1, score, player1} ->
	    broadcast(sessions,  {:ball, :hide})
	    broadcast(sessions,  {:player1, :score, score})
	    :timer.send_after(@serve, self(), {:serve, :player1})	    
	    pong(player1, player2, :na, sessions)

	  {:score, :player2, score, player2} ->
	    broadcast(sessions,  {:ball, :hide})
	    broadcast(sessions,  {:player2, :score, score})
	    :timer.send_after(@serve, self(), {:serve, :player2})	    
	    pong(player1, player2, :na, sessions)
	    
	end

      {:player1, :close} ->
	:io.format("player 1 closed connectio\n" )
	:ok

      {:player2, :close} ->
	:io.format("player 2 closed connectio\n" )
	:ok

      {:frw, msg} ->
	## if we want to send anything else to the sessions
	:io.format("msg to players: ~w\n", [msg])
	broadcast(sessions, {:frw, msg})
	pong(player1, player2, ball, sessions)


      :update ->
	Pong.pong(player1, player2, ball, sessions)
	
      :stop ->
	:ok

      strange ->
	:io.format("pong: received strange message ~w~n", [strange])
	pong(player1, player2, ball, sessions)	
    end
  end

	
    


end
