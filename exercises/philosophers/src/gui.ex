defmodule Gui do

  @red {200, 0, 0}
  @yellow {255,255,0}
  @blue {0, 0, 200}  
  @black {0, 0, 0}
  @white {1, 1, 1}

  

  def start(name) do
    spawn_link(fn() -> init(name) end)
  end

  def init(name) do
    width = 400
    height = 400
    server = :wx.new()  #Server will be the parent for the Frame
    frame = :wxFrame.new(server, -1, name, [{:size,{width, height}}])
    :wxFrame.setBackgroundColour(frame, @white)
    :wxFrame.show(frame)
    loop(frame)
  end
	

  def loop(frame) do
    receive do
      :waiting ->
	:wxFrame.setBackgroundColour(frame, @yellow)
	loop(frame)
      :enter ->
	:wxFrame.setBackgroundColour(frame, @red)
	loop(frame)
      :leave ->
	:wxFrame.setBackgroundColour(frame, @blue)
	loop(frame)
      :abort ->
	:wxFrame.setBackgroundColour(frame, @black)
	loop(frame)
      :stop ->
	:ok
      error ->
	:io.format("gui: strange message ~w ~n", [error])
	loop(frame)
    end
  end

end


