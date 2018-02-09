defmodule Bench do

  def server() do
    server(:waves, :small, 255, "wave.ppm")
  end

  def server(img, size, depth, name) do
    {x, y, xn} = image(img)
    {width, height, k} = size(size, x, xn)
    {:ok, server} = Server.start(width, height, x, y, k, depth, name)
    :global.re_register_name(:server, server)
  end

  def sky(img, size, depth) do 
    parallel(1, img, size, depth)
    parallel(2, img, size, depth)    
    parallel(4, img, size, depth)
    parallel(6, img, size, depth)
    parallel(8, img, size, depth)
    parallel(12, img, size, depth)
    parallel(16, img, size, depth)
  end

  def pro(img, size, depth) do
    parallel(1, img, size, depth)
    parallel(2, img, size, depth)    
    parallel(3, img, size, depth)    
    parallel(4, img, size, depth)
  end

  
  def parallel(n, img, size, depth) do
    :erlang.system_flag(:schedulers_online, n)
    IO.write("#{n} cores -- ")
    parallel(img, size, depth)
  end

  def parallel(img, size, depth) do
    {t, _} = :timer.tc( fn() -> 
      {x, y, xn} = image(img)
      {width, height, k} = size(size, x, xn)
      Mandelp.mandelbrot(width, height, x, y, k, depth)
    end)
    IO.write("#{trunc(t/1000)} ms\n")
  end

  def bench(img, size, depth) do
    {t, _} = :timer.tc( fn() -> 
      {x, y, xn} = image(img)
      {width, height, k} = size(size, x, xn)
      Mandel.mandelbrot(width, height, x, y, k, depth)
    end)
    IO.write("#{trunc(t/1000)} ms\n")
  end  

  ### Given the upper left corner, the size information, the depth and a
  ### name this procedure will calculate an image and print it to a .ppm
  ### file.

  
  def print({x, y}, {width, height, k}, depth, file) do
    {t, _} = :timer.tc( fn() ->
      image = Mandelp.mandelbrot(width, height, x, y, k, depth)
      PPM.write(file, image)
    end)
    IO.puts("picture generated and printed in #{trunc(t/1000)} ms")
  end
 
  ### size(size, x, xn) ::  {width, height, k} where size is :small,
  ### :large or :huge, and x, xn are the left- and rightermost
  ### values. The function will return a width and height value that
  ### has the 16:9 ratio. The k value is the step factor for each
  ### pixle.

  def size(:small, x, x1) do
    width = 960
    height = 540
    k = (x1 - x)/width
    {width, height, k}
  end
  def size(:large, x,x1) do
    width = 1920
    height = 1080
    k = (x1 - x)/width
    {width, height, k}
  end
  def size(:long, x, x1) do
    width = 2560
    height = 1080
    k = (x1 - x)/width
    {width, height, k}
  end
  def size(:huge, x, x1) do
    width = 3840
    height = 2160
    k = (x1 - x)/width
    {width, height, k}
  end


  ### image(name) -> {x,y,x1} where x,y is the upper left corner and
  ### x1 the rightermost position.

  def image(:mandel) do
    {-2.6,1.2,1.6}
  end
  def image(:waves) do
    {-0.14,0.85,-0.13}
  end
  def image(:forest) do
    {-0.136,0.85,-0.134}
  end

end

    




    
	

    
