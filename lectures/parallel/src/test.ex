defmodule Test do


  def batch() do
    t0 = :erlang.monotonic_time(:millisecond)
    PPM.read("hockey.ppm") |>
      Batch.map(Filter.rgb_to_gray()) |>
      Batch.map(Filter.gray_reduce()) |>
      Batch.map(Filter.gray_edge()) |>
      Batch.map(Filter.gray_invert()) |>           
      PPM.write("batch.ppm")
    t1 = :erlang.monotonic_time(:millisecond)
    IO.puts("  total of  #{t1-t0} ms")        
  end




  def para() do
    t0 = :erlang.monotonic_time(:millisecond)
    PPM.read("hockey.ppm") |>
      Para.map(Filter.rgb_to_gray()) |>
      Para.map(Filter.gray_reduce()) |>
      Para.map(Filter.gray_edge()) |>
      Para.map(Filter.gray_invert()) |>           
      PPM.write("batch.ppm")
    t1 = :erlang.monotonic_time(:millisecond)
    IO.puts("  total of  #{t1-t0} ms")        
  end

  

  def stream() do
    t0 = :erlang.monotonic_time(:millisecond)	
    self() |>
    PPM.writer("stream.ppm") |>
      Strm.start(Filter.gray_invert()) |>
      Strm.start(Filter.gray_edge()) |>
      Strm.start(Filter.gray_reduce()) |>
      Strm.start(Filter.rgb_to_gray() ) |>
      PPM.reader("hockey.ppm")
    t1 = receive do
           :done ->
   	     :erlang.monotonic_time(:millisecond)		  
         end 
    IO.puts("  total of  #{t1-t0} ms")        
  end

  

  
end

