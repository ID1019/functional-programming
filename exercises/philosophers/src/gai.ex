defmodule Gai do

  @red {200, 0, 0}
  @yellow {255,255,0}
  @blue {0, 0, 200}  
  @black {0, 0, 0}
  @white {255, 255, 255}
  

  def start(names) do
    spawn_link(fn() -> init(names) end)
  end

  def init(names) do
    width = 800
    height = 800
    server = :wx.new()  #Server will be the parent for the Frame
    frame = :wxFrame.new(server, -1, "Philosophers", [{:size,{width, height}}])
    :wxFrame.setBackgroundColour(frame, @white)
    bitmap = :wxBitmap.new(width,height)
    :wxFrame.show(frame)
    state = state(names)
    pos = positions(names, 400)
    :io.format("positions: ~w\n", [pos])
    :io.format("state: ~w\n", [state])
    loop(frame, bitmap, state, pos)
  end
	

  def loop(frame, bitmap, state, pos) do
    receive do
       {:action, name, action} ->
	state = update(state, name, action)
	:io.format("state: ~w\n", [state])
	draw(frame, bitmap, state, pos)
	loop(frame, bitmap, state, pos)
      :stop ->
	:ok
      error ->
	:io.format("gui: strange message ~w ~n", [error])
	loop(frame, bitmap, state, pos)
    end
  end

  def state(names) do
    Enum.map(names, fn(name) -> {name, :done} end)
  end

  def update(names, name, action) do
    Keyword.replace(names, name, action)
  end
  
  
  def positions(names, off) do
    d = length(names)
    pos = Enum.map(1..d,  fn(r) ->{round(:math.sin((2*:math.pi / d)*r)*200+off),round(:math.cos((2*:math.pi/d)*r)*200+off)} end)
    List.zip([names, pos])
  end
  

  def draw(frame, bitmap, names, pos) do

    memDC = :wxMemoryDC.new(bitmap)

    white = :wxBrush.new()
    :wxBrush.setColour(white, @white)
    
    :wxDC.setBackground(memDC,white)

    :wxDC.clear(memDC)

    canDC = :wxWindowDC.new(frame)
    
    Enum.each(names, fn({name, state}) ->
      xy = pos[name]
      brush = :wxBrush.new()
      color = case state do
		:waiting -> @yellow
		:enter -> @red
		:leave -> @blue
		:died -> @black
		:done -> @white
	      end
      :wxBrush.setColour(brush, color)
      :wxDC.setBrush(memDC,brush)
      :wxDC.drawCircle(memDC, xy, 40)
      :wxDC.blit(canDC, {0,0}, {:wxBitmap.getWidth(bitmap), :wxBitmap.getHeight(bitmap)}, memDC, {0,0})
    end)

    :wxWindowDC.destroy(canDC)
    :wxMemoryDC.destroy(memDC)

    :ok
  end
      
end


