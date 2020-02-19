defmodule Pong do

  @tick    100
  @serve  4000
  
  def start(port) do
    spawn(fn() -> init(port) end)
  end

  defp init(port) do

    pong = self()
    ses1 = Session.start(:player1, pong)
    ses2 = Session.start(:player2, pong)
    
    server = WebSocket.start(port, [ses1, ses2])


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

      {:player1, :close} ->
	:io.format("player 1 closed connectio\n" )
	:ok

      {:player2, :close} ->
	:io.format("player 2 closed connectio\n" )
	:ok

      {:frw, msg} ->
	## if we want to send anything else to the sessions
	:io.format("msg to players: ~w\n", [msg])
	broadcast(sessions, msg)
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
