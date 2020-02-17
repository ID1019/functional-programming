defmodule Pong do

  # The field 
  @field_width   700
  @field_height  600

  # A paddle
  @paddle_width 20
  @paddle_height 100   ## multiple of 5 
  @paddle_speed 5

  # A ball, deafult server delta, deafult tick time in ms
  @dx  10
  @dy  0
  @tick 100
  @ball_width 20
  @serve  4000
  
  
  def start(port) do
    spawn(fn() -> init(port) end)
  end
  

  defp init(port) do

    pong = self()
    ses1 = Session.start(:player1, pong)
    ses2 = Session.start(:player2, pong)
    
    server = WebSocket.start(port, [ses1, ses2])

    y = trunc( (@field_height - @paddle_height) / 2)

    receive do
      {:ready, name} ->
	:io.format("~w ready~n", [name])
	receive do
	  {:ready, name} ->
	    :io.format("~w ready~n", [name])
	    :timer.send_after(@serve, self(), {:serve, :player1})
	    pong({:player, y, 0}, {:player, y, 0}, :ball, [ses1, ses2])
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
	#:io.format("msg down from player 1\n")
	{:player, y1, s1}= player1
	if (y1 + @paddle_height) < @field_height do
	  broadcast(sessions, {:player1, :down})
	  pong({:player, y1 + @paddle_speed, s1}, player2, ball, sessions)
	else
	  pong(player1, player2, ball, sessions)
	end

      {:player1, :up}->
	#:io.format("msg down from player 1\n")
	{:player, y1, s1}= player1
	if y1 > 0 do
	    broadcast(sessions, {:player1, :up})
	    pong({:player, y1 - @paddle_speed, s1}, player2, ball, sessions)
	else
	  pong(player1, player2, ball, sessions)
	end

      {:player2, :down} ->
	#:io.format("msg down from player 2\n")
	{:player, y2, s2}= player2
	if  (y2 + @paddle_height) < @field_height do
	  broadcast(sessions, {:player2, :down})
	  pong(player1, {:player, y2 + @paddle_speed, s2}, ball, sessions)
	else
	  pong(player1, player2, ball, sessions)
	end

      {:player2, :up} ->
	#:io.format("msg down from player 2\n")
	{:player, y2, s2}= player2
	if y2 > 0 do
	    broadcast(sessions, {:player2, :up})
	    pong(player1, {:player, y2 - @paddle_speed, s2}, ball, sessions)
	else
	  pong(player1, player2, ball, sessions)
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
	broadcast(sessions, msg)
	pong(player1, player2, ball, sessions)


      {:serve, turn} ->
	{bx, by, dx, dy}  = serve(turn, player1, player2)
	tick = @tick
	:timer.send_after(tick, self(), :tick)
	broadcast(sessions, {:ball, bx, by})	
	pong(player1, player2, {:ball, tick, bx, by, dx, dy}, sessions)

      :tick ->

	{:player, y1, s1}= player1
	{:player, y2, s2}= player2
	
	{:ball, tick, bx, by, dx, dy} = ball

	bx = bx + dx
	by = by + dy

	#:io.format("ball at: ~w ~w\n", [bx, by])
	
	cond  do
	  (dx > 0 and (bx >= (@field_width - 2*@paddle_width - @ball_width)) and (by >= (y2-@ball_width) and by <= (y2 + @paddle_height)))->
	    #:io.format(" ball hits right paddle at: ~w ~w\n", [bx, by])
	    {dx, dy} = spin(y2, by, dx, dy)
	    tick = if (tick > 10) do tick - 1 else tick end
	    :timer.send_after(tick, self(), :tick)
	    broadcast(sessions, {:ball, bx, by})
	    pong(player1, player2, {:ball, tick, bx, by, dx, dy}, sessions)
	    
	  (dx < 0 and (bx <= (2*@paddle_width)) and  (by >= (y1-@ball_width) and by <= (y1 + @paddle_height))) ->
	    #:io.format(" ball hits left paddle\n")
	    {dx, dy} = spin(y1, by, dx, dy)
	    tick = if (tick > 10) do tick - 1 else tick end
	    :timer.send_after(tick, self(), :tick)
	    broadcast(sessions, {:ball, bx, by})
	    pong(player1, player2, {:ball, tick, bx, by, dx, dy}, sessions)

	  (bx >= (@field_width - @ball_width)) ->
	    s1 = s1+1
	    bx = 0
	    by = 0
	    :timer.send_after(@serve, self(), {:serve, :player1})
	    broadcast(sessions, {:player1, :score, s1})
	    broadcast(sessions, {:ball, bx, by})		  
	    pong({:player, y1, s1}, player2, :ball, sessions)

	 (bx <= 0) ->
	    s2 = s2+1
	    bx = 0
	    by = 0
	    :timer.send_after(@serve, self(), {:serve, :player2})
	    broadcast(sessions,  {:player2, :score, s2})
	    broadcast(sessions,  {:ball, bx, by})		
	    pong(player1, {:player, y2, s2}, :ball, sessions)

	 (by <= 0) ->
            dy = -dy
    	    bx = bx + dx
	    by = by + dy	
	    :timer.send_after(tick, self(), :tick)
	    broadcast(sessions,  {:ball, bx, by})  
	    pong(player1, player2, {:ball, tick, bx, by, dx, dy}, sessions)

	 (by >= (@field_height - @ball_width)) ->
            dy = -dy
    	    bx = bx + dx
	    by = by + dy	
	    :timer.send_after(tick, self(), :tick)
	    broadcast(sessions,  {:ball, bx, by})
	    pong(player1, player2, {:ball, tick, bx, by, dx, dy}, sessions)

	 true -> 
    	    bx = bx + dx
	    by = by + dy	
	    :timer.send_after(tick, self(), :tick)
	    broadcast(sessions,  {:ball, bx, by})
	    pong(player1, player2,  {:ball, tick, bx, by, dx, dy}, sessions)	
	end

      :stop ->
	:ok

      :update ->
	Pong.pong(player1, player2, ball, sessions)

      strange ->
	:io.format("received strange message ~w~n", [strange])
	pong(player1, player2, ball, sessions)	
    end
  end

  def serve(:player1,{:player, y, _}, _) do
    dx = @dx
    dy = @dy
    bx = (2 * @paddle_width) 	       
    by = (y + trunc(@paddle_height/2))
    {bx, by, dx, dy}
  end
  def serve(:player2, _, {:player, y, _}) do
    dx = -@dx
    dy = @dy
    bx = (@field_width - 2*@paddle_width - @ball_width) 	    
    by = (y + trunc(@paddle_height/2))
    {bx, by, dx, dy}
  end  
	
  
  
  def spin(y, by, dx, dy) do
    hit = by - y

    cond do
      hit < trunc((@paddle_height * 0.2)) ->
	{-dx, dy-4}  # change direction
      hit <  trunc((@paddle_height * 0.4)) ->
	{-dx, dy-2}  # change direction	
      hit <  trunc((@paddle_height * 0.6)) ->
	{-dx, dy}    # change direction
      hit >  trunc((@paddle_height * 0.8)) ->
	{-dx, dy+2}  # change direction
      true ->
	{-dx, dy+4}   # 
    end
    
  end




  


end
