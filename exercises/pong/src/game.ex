defmodule Game do

  # the field 
  @field_width   700
  @field_height  600

  # a paddle
  @paddle_width 20
  @paddle_height 100  
  @paddle_speed 5

  # a ball
  @ball_width 20
  @dy  0
  @dx 10

  
  
  def player1(name) do
    x = (2 * @paddle_width)
    y = trunc( (@field_height - @paddle_height) / 2)
    {name, x, y, @dx}
  end

  def player2(name) do
    x = (@field_width - 2*@paddle_width - @ball_width)
    y = trunc( (@field_height - @paddle_height) / 2)
    {name, x, y, -@dx}
  end
  
  def down({name, x, y, d}) do
    if (y + @paddle_height) < @field_height do
      {:ok, {name, x, y + @paddle_speed, d}}
    else
      :no
    end
  end

  def up({name, x, y, d}) do
    if y > 0 do
      {:ok, {name, x, y - @paddle_speed, d}}
    else
      :no
    end
  end

  def serve({_name, x, y, d}) do
    dx = d
    dy = @dy
    bx = x
    by = (y + trunc(@paddle_height/2))
    {{bx, by}, {:ball, bx, by, dx, dy}}
  end

  def move_ball(player1, player2, ball) do

    {:ball, bx, by, dx, dy}  = ball
    {name1, x1, y1, _} = player1
    {name2, x2, y2, _} = player2
      
    bx = bx + dx
    by = by + dy
	  
    cond  do
      dx > 0 and (bx >= x2) and (by >= (y2-@ball_width) and by <= (y2 + @paddle_height)) ->
	#:io.format(" ball hits right paddle at: ~w ~w\n", [bx, by])
	{dx, dy} = spin(y2, by, dx, dy)
	{:bounce, {bx, by}, {:ball, bx, by, dx, dy}}

      dx < 0 and (bx <= x1) and  (by >= (y1-@ball_width) and by <= (y1 + @paddle_height)) ->
	#:io.format(" ball hits left paddle\n")
	{dx, dy} = spin(y1, by, dx, dy)
	{:bounce, {bx, by}, {:ball, bx, by, dx, dy}}

      bx >= (@field_width - @ball_width) ->
	{:score, name1}
	  
      bx <= 0 ->
	{:score, name2}

      by <= 0 ->
        dy = -dy
    	bx = bx + dx
	by = by + dy	
	{:moved, {bx, by}, {:ball, bx, by, dx, dy}}

      by >= (@field_height - @ball_width) ->
        dy = -dy
    	bx = bx + dx
	by = by + dy	
	{:moved, {bx, by}, {:ball, bx, by, dx, dy}}

      true ->
    	bx = bx + dx
	by = by + dy	
	{:moved, {bx, by}, {:ball, bx, by, dx, dy}}
    end
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
