defmodule Test do

  def demo() do
    small(-2.6, 1.2, 1.2)
  end

  def small(x0, y0, xn) do
    width = 960
    height = 540
    depth = 64
    k = (xn - x0) / width
    image = Mandel.mandelbrot(width, height, x0, y0, k, depth)
    PPM.write("small.ppm", image)
  end

  def julia() do
    julia(-1.5, 1, 1.5)
  end  
  
  def julia(x0, y0, xn) do
    width = 3840
    height = 2160
    depth = 1024
    k = (xn - x0) / width
    image = Julia.julia(width, height, x0, y0, k, depth)
    PPM.write("julia.ppm", image)
  end
  
end
