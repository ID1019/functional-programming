defmodule Server do

  def small() do
    width = 960
    height = 540
    x0 = -0.136
    y0 = 0.85
    xn = -0.134
    k = (xn - x0)/ width
    depth = 255
    start(width, height, x0, y0 , k, depth, "forest.ppm") 
  end

  def large() do
    width = 1920
    height = 1080
    x0 = -0.136
    y0 = 0.85
    xn = -0.134
    k = (xn - x0)/ width
    depth = 255
    start(width, height, x0, y0 , k, depth, "forest.ppm") 
  end

  def huge() do
    width = 3840
    height = 2160
    x0 = -0.136
    y0 = 0.85
    xn = -0.134
    k = (xn - x0)/ width
    depth = 255
    start(width, height, x0, y0 , k, depth, "forest.ppm") 
  end  
  
  # The start/7 process will start a mandel server and a print process.
  def start(width, height, x, y, k, depth, file) do
    {:ok, spawn(fn -> init(width, height, x, y, k, depth, file) end)}
  end

  defp init(width, height, x, y, k, depth, file) do
    {:ok, ctrl} = Print.start(file, width, height)

    # Sending lambda expressions works if client side has exactly the
    # same code base. We try to avoid this when doing it in class.
    # trans = fn(w, h) -> {x + k * (w - 1), y - k * (h - 1)} end
    trans = {:trans, x, y, k}

    rows(width, height, trans, depth, ctrl)
  end

  defp rows(_, 0, _, _, _) do
    IO.puts("Done!")
    done()
  end
  defp rows(w, h, tr, depth, ctrl) do
    receive do
      {:request, from} ->
        IO.write("Sending request {#{w}, #{h}} to ")
        IO.inspect(from)
        send(from, {:task, w, h, tr, depth, ctrl})
        send(ctrl, :go)
        rows(w, h - 1, tr, depth, ctrl)

      :stop ->
        :ok

      strange ->
        IO.puts("Strange message #{strange}")
        rows(w, h, tr, depth, ctrl)
    end
  end

  defp done() do
    receive do
      {:request, from} ->
        send(from, :done)
        done()

      :stop ->
        :ok
    end
  end

end
