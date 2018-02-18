defmodule Test do

  def stream() do
    t0 = :erlang.monotonic_time(:millisecond)
    ctrl = self()
    writer = PPM.writer("batch.ppm", ctrl)
    conv0 = Stream.start(1, rgb_to_gray(), gray(), writer)     
    PPM.reader("stockholm.ppm", conv0)
    t1 = receive do
           :done ->
   	     :erlang.monotonic_time(:millisecond)	
         end 
    IO.puts("  total of  #{t1-t0} ms")        
  end


  def batch() do
    t0 = :erlang.monotonic_time(:millisecond)
    image = PPM.read("stockholm.ppm")
    image = Batch.map(image, 1, rgb_to_gray(), gray())
    PPM.write(image, "stream.ppm")
    t1 = :erlang.monotonic_time(:millisecond)
    IO.puts("  total of  #{t1-t0} ms")        
  end


  ## 1x1 kernels

  def rgb_to_gray() do
    fn({:rgb, {w, h}, 255}) ->
      {:gray, {w, h}, 255}
    end
  end

  def gray() do
    fn({r,g,b}) ->
      div(r+g+b, 3)
    end
  end

  
  ## 3x3 kernels 

  def null() do
    fn(header) ->
      header
    end
  end  
  
  
  def edge() do
    fn(lines) -> Kern.fold([ 0, 1, 0,
			     1,-4, 1,
			     0, 1, 0], lines, {0,0,0}) end
  end

  
  def sharp() do
    fn(lines) -> Kern.fold([ 0,-1, 0,
			    -1, 5,-1,
			     0,-1, 0], lines, {0,0,0}) end
  end  

  ## 5x5 kernels
  
  def blur() do
    fn(lines) -> Kern.fold([ 0.04, 0.04, 0.04, 0.04, 0.04,
			     0.04, 0.04, 0.04, 0.04, 0.04,
			     0.04, 0.04, 0.04, 0.04, 0.04,
			     0.04, 0.04, 0.04, 0.04, 0.04,
			     0.04, 0.04, 0.04, 0.04, 0.04], lines, {0,0,0}) end
  end
  

  
end

