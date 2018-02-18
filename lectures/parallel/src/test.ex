defmodule Test do

  def stream() do
    t0 = :erlang.monotonic_time(:millisecond)
    ctrl = self()
    writer = PPM.writer("batch.ppm", ctrl)
    conv0 = Stream.start(rgb_to_gray(), writer)     
    PPM.reader("hockey.ppm", conv0)
    t1 = receive do
           :done ->
   	     :erlang.monotonic_time(:millisecond)	
         end 
    IO.puts("  total of  #{t1-t0} ms")        
  end


  def batch() do
    t0 = :erlang.monotonic_time(:millisecond)
    image = PPM.read("hockey.ppm")
    image = Batch.map(image, rgb_edge())
    PPM.write(image, "stream.ppm")
    t1 = :erlang.monotonic_time(:millisecond)
    IO.puts("  total of  #{t1-t0} ms")        
  end


  ## 1x1 kernels

  def rgb_to_gray() do
    fn({:rgb, size, depth}) ->
      {:ok, 1,
       {:gray, size, depth}, 
       fn({r,g,b}) ->
	 div(r+g+b, 3)
       end}
    end
  end

  
  ## 3x3 kernels 

  def rgb_edge() do
    fn ({:rgb, size, depth}) ->
      {:ok, 3,
       {:rgb, size, depth},
       fn(lines) -> Kern.fold([ 0, 1, 0,
  			        1,-4, 1,
			        0, 1, 0], lines, {0,0,0}) end}
    end
  end

  
  def rgb_sharp() do
    fn ({:rgb, size, depth}) ->
      {:ok, 3,
       {:rgb, size, depth},
       fn(lines) -> Kern.fold([  0,-1, 0,
				-1, 5,-1,
			         0,-1, 0], lines, {0,0,0}) end}
    end
  end  

  ## 5x5 kernels
  
  def rgb_blur() do
    fn ({:rgb, size, depth}) ->
      {:ok, 5,
       {:rgb, size, depth},       
       fn(lines) -> Kern.fold([ 0.04, 0.04, 0.04, 0.04, 0.04,
			        0.04, 0.04, 0.04, 0.04, 0.04,
			        0.04, 0.04, 0.04, 0.04, 0.04,
			        0.04, 0.04, 0.04, 0.04, 0.04,
			        0.04, 0.04, 0.04, 0.04, 0.04], lines, {0,0,0}) end}
    end
  end
  

  
end

