defmodule Gai do

  @red {200, 0, 0}
  @yellow {255,255,0}
  @blue {0, 0, 200}  
  @black {0, 0, 0}
  @white {255, 255, 255}

  

  def start(name) do
    spawn_link(fn() -> init(name) end)
  end

  def init(name) do
    width = 400
    height = 400
    server = :wx.new()  #Server will be the parent for the Frame
    frame = :wxFrame.new(server, -1, name, [{:size,{width, height}}])
    :wxFrame.setBackgroundColour(frame, @white)
    bitmap = :wxBitmap.new(width,height)
    :wxFrame.show(frame)
    loop(frame, bitmap)
  end
	

  def loop(frame, bitmap) do
    receive do
      :waiting ->
	draw(frame, bitmap)
	loop(frame, bitmap)
      :enter ->
	:wxFrame.setBackgroundColour(frame, @red)
	loop(frame, bitmap)
      :leave ->
	:wxFrame.setBackgroundColour(frame, @blue)
	loop(frame, bitmap)
      :abort ->
	:wxFrame.setBackgroundColour(frame, @black)
	loop(frame, bitmap)
      :stop ->
	:ok
      error ->
	:io.format("gui: strange message ~w ~n", [error])
	loop(frame, bitmap)
    end
  end


  def draw(frame, bitmap) do

    memDC = :wxMemoryDC.new(bitmap)

    white = :wxBrush.new()
    :wxBrush.setColour(white, @white)
    
    :wxDC.setBackground(memDC,white)

    :wxDC.clear(memDC)
    
    brush = :wxBrush.new()
    :wxBrush.setColour(brush, @red)
    :wxDC.setBrush(memDC,brush)

    :wxDC.drawCircle(memDC, {100, 100}, 20)
    canDC = :wxWindowDC.new(frame)
    :wxDC.blit(canDC, {0,0}, {:wxBitmap.getWidth(bitmap), :wxBitmap.getHeight(bitmap)}, memDC, {0,0})

    :wxWindowDC.destroy(canDC)
    :wxMemoryDC.destroy(memDC)
  end
  
end


