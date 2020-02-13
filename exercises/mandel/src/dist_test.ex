defmodule DistTest do

  def server(img, size, depth, name) do
    file = name <> ".ppm"
    {x, y, x1} = image(img)
    {width, height, k} = size(size, x, x1)
    {:ok, server} = Server.start(width, height, x, y, k, depth, file)
    Process.register(server, :server)
  end

  defp sky(img, size, depth) do
    bench(1, img, size, depth)
    bench(2, img, size, depth)
    bench(4, img, size, depth)
    bench(6, img, size, depth)
    bench(8, img, size, depth)
    bench(12, img, size, depth)
    bench(16, img, size, depth)
  end

  defp pro(img, size, depth) do
    bench(1, img, size, depth)
    bench(2, img, size, depth)
    bench(3, img, size, depth)
    bench(4, img, size, depth)
  end

  def bench(n, img, size, depth) do
    :erlang.system_flag(:schedulers_online, n)
    IO.puts("#{n} cores")
    s = :erlang.system_time(:milli_seconds)
    {x, y, x1} = image(img)
    {width, height, k} = size(size, x, x1)
    DistMandel.mandelbrot(width, height, x, y, k, depth)
    t = :erlang.system_time(:milli_seconds) - s
    IO.puts("#{t} ms")
  end

  # Given the upper left corner, the size information, the depth and a
  # name this procedure will calculate an image and print it to a .ppm
  # file.
  def print({x, y}, {width, height, k}, depth, name) do
    file = name <> ".ppm"
    t0 = :erlang.system_time(:milli_seconds)
    image = DistMandel.mandelbrot(width, height, x, y, k, depth)
    PPM.write(file, image)
    t = :erlang.system_time(:milli_seconds) - t0
    IO.puts("Picture generated and printed in #{t} ms")
  end

  # size(size, x, x1) -> {width, height, k} where size is small, large
  # or huge, and x, x1 are teh left- and rightermost values. The
  # function will return a Width and Height value taht has the 16:9
  # ratio. The k value is the step factor for each pixle. 
  defp size("small", x, x1) do
    width = 960
    height = 540
    k = (x1 - x) / width
    {width, height, k}
  end
  defp size("large", x, x1) do
    width = 1920
    height = 1080
    k = (x1 - x) / width
    {width, height, k}
  end
  defp size("long", x, x1) do
    width = 2560
    height = 1080
    k = (x1 - x) / width
    {width, height, k}
  end
  defp size("huge", x, x1) do
    width = 3840
    height = 2160
    k = (x1 - x) / width
    {width, height, k}
  end

  # image("name") -> {x, y, x1} where x, y is the upper left corner and x1
  # the rightermost position.
  defp image("mandel") do
    {-2.6, 1.2, 1.6}
  end
  defp image("waves") do
    {-0.14, 0.85, -0.13}
  end
  defp image("forest") do
    {-0.136, 0.85, -0.134}
  end

end
