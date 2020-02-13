defmodule DistTest do

  def server(img, size, depth, name) do
    file = name <> ".ppm"
    {x, y, x1} = image(img)
    {width, height, k} = size(size, x, x1)
    {:ok, server} = Server.start(width, height, x, y, k, depth, file)
    Process.register(server, :server)
  end


  # size(size, x, x1) -> {width, height, k} where size is small, large
  # or huge, and x, x1 are teh left- and rightermost values. The
  # function will return a Width and Height value taht has the 16:9
  # ratio. The k value is the step factor for each pixle. 

  defp size(:small, x, x1) do
    width = 960
    height = 540
    k = (x1 - x) / width
    {width, height, k}
  end
  defp size(:large, x, x1) do
    width = 1920
    height = 1080
    k = (x1 - x) / width
    {width, height, k}
  end
  defp size(:long, x, x1) do
    width = 2560
    height = 1080
    k = (x1 - x) / width
    {width, height, k}
  end
  defp size(:huge, x, x1) do
    width = 3840
    height = 2160
    k = (x1 - x) / width
    {width, height, k}
  end

  # image(:name) -> {x, y, x1} where x, y is the upper left corner and x1
  # the rightermost position.
  defp image(:mandel) do
    {-2.6, 1.2, 1.6}
  end
  defp image(:waves) do
    {-0.14, 0.85, -0.13}
  end
  defp image(:forest) do
    {-0.136, 0.85, -0.134}
  end

end
