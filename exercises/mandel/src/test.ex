defmodule Test do

  def demo() do
    img = small(-2.6, 1.2, 1.7)
    PPM.write("demo.ppm", img)    
  end

  def forest() do
    img = large(-0.136, 0.85, -0.134)
    PPM.write("forest.ppm", img)
  end

  def waves() do
    img = large(-0.14,0.85,-0.13)
    PPM.write("waves.ppm", img)
  end  
  
  
  def small(x0, y0, xn) do
    width = 960
    height = 540
    depth = 64
    k = (xn - x0) / width
    Mandel.mandelbrot(width, height, x0, y0, k, depth)
  end
  
  def large(x0,y0,xn) do
    width = 1920
    height = 1080
    depth = 255
    k = (xn - x0)/ width
    Mandel.mandelbrot(width, height, x0, y0, k, depth)
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
