defmodule Test do

  def stream() do
    t0 = :erlang.monotonic_time(:millisecond)	
    ctrl = self()
    out = PPM.writer("motion.ppm", ctrl)
    out = Strm.start(Filter.rgb_motion(), out)
    out = Strm.start(Filter.rgb_motion(), out)         
    PPM.reader("hockey.ppm", out)
    t1 = receive do
           :done ->
   	     :erlang.monotonic_time(:millisecond)		  
         end 
    IO.puts("  total of  #{t1-t0} ms")        
  end


  def batch() do
    t0 = :erlang.monotonic_time(:millisecond)
    image = PPM.read("hockey.ppm")
    image = Batch.map(image, Filter.rgb_to_gray())
    image = Batch.map(image, Filter.gray_reduce())
    image = Batch.map(image, Filter.gray_edge())
    image = Batch.map(image, Filter.gray_invert())            
    PPM.write(image, "batch.ppm")
    t1 = :erlang.monotonic_time(:millisecond)
    IO.puts("  total of  #{t1-t0} ms")        
  end


  

  
end

