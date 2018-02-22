defmodule Bench do

  ## batch processing read -> gray -> reduced -> invert -> write
  
  def stream() do
    t0 = :erlang.monotonic_time(:millisecond)
    ctrl = self()
    out = PPM.writer("stream.ppm", ctrl)
    out = Strm.start(Filter.gray_invert(), out)             
    out = Strm.start(Filter.gray_reduce(), out)     
    out = Strm.start(Filter.rgb_to_gray(), out)     
    PPM.reader("hockey.ppm", out)
    t1 = receive do
           :done ->
   	     :erlang.monotonic_time(:millisecond)	
         end 
    IO.puts("  total of  #{t1-t0} ms")        
  end

  ## batch processing read ->  gray -> reduced -> invert -> write 
  def batch() do
    t0 = :erlang.monotonic_time(:millisecond)

    image = PPM.read("hockey.ppm")
    t1 = :erlang.monotonic_time(:millisecond)
    IO.puts("   read in  #{t1-t0} ms")        

    image = Batch.map(image, Filter.rgb_to_gray())
    t2 = :erlang.monotonic_time(:millisecond)
    IO.puts("   gray in  #{t2-t1} ms")        

    image = Batch.map(image, Filter.gray_reduce())
    t3 = :erlang.monotonic_time(:millisecond)
    IO.puts("reduced in  #{t3-t2} ms")        

    image = Batch.map(image, Filter.gray_invert())            
    t4 = :erlang.monotonic_time(:millisecond)
    IO.puts(" invert in  #{t4-t3} ms")        

    PPM.write(image, "batch.ppm")
    t5 = :erlang.monotonic_time(:millisecond)
    IO.puts("  write in  #{t5-t4} ms")        

    IO.puts("  total of  #{t5-t0} ms")        
  end


  def blur_stream() do
    t0 = :erlang.monotonic_time(:millisecond)
    ctrl = self()
    out = PPM.writer("blur.ppm", ctrl)
    out = Strm.start(Filter.rgb_blur(), out)             
    out = Strm.start(Filter.rgb_blur(), out)     
    out = Strm.start(Filter.rgb_blur(), out)     
    PPM.reader("hockey.ppm", out)
    t1 = receive do
           :done ->
   	     :erlang.monotonic_time(:millisecond)	
         end 
    IO.puts("  total of  #{t1-t0} ms")        
  end


  ## batch processing read ->  gray -> reduced -> invert -> write 
  def blur_batch() do
    t0 = :erlang.monotonic_time(:millisecond)

    image = PPM.read("hockey.ppm")
    t1 = :erlang.monotonic_time(:millisecond)
    IO.puts("   read in  #{t1-t0} ms")        

    image = Batch.map(image, Filter.rgb_blur())
    t2 = :erlang.monotonic_time(:millisecond)
    IO.puts("   blur in  #{t2-t1} ms")        

    image = Batch.map(image, Filter.rgb_blur())
    t3 = :erlang.monotonic_time(:millisecond)
    IO.puts("   blur in  #{t3-t2} ms")        

    image = Batch.map(image, Filter.rgb_blur())
    t4 = :erlang.monotonic_time(:millisecond)
    IO.puts("   blur in  #{t4-t3} ms")        

    image = Batch.map(image, Filter.rgb_blur())
    t5 = :erlang.monotonic_time(:millisecond)
    IO.puts("   blur in  #{t5-t4} ms")        

    PPM.write(image, "batch.ppm")
    t6 = :erlang.monotonic_time(:millisecond)
    IO.puts("  write in  #{t6-t5} ms")        

    IO.puts("  total of  #{t6-t0} ms")        
  end
  

  def bananaz() do
    t0 = :erlang.monotonic_time(:millisecond)
    ctrl = self()
    out = PPM.writer("blur.ppm", ctrl)
    out = Strm.start(Filter.rgb_blur(), out)             
    out = Strm.start(Filter.rgb_blur(), out)     
    out = Strm.start(Filter.rgb_blur(), out)
    out = Strm.start(Filter.rgb_blur(), out)
    out = Strm.start(Filter.rgb_blur(), out)
    out = Strm.start(Filter.rgb_blur(), out)
    out = Strm.start(Filter.rgb_blur(), out)
    out = Strm.start(Filter.rgb_blur(), out)         
    PPM.reader("hockey.ppm", out)
    t1 = receive do
           :done ->
   	     :erlang.monotonic_time(:millisecond)	
         end 
    IO.puts("  total of  #{t1-t0} ms")
  end

end
