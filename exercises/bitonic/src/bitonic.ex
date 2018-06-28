defmodule Bitonic do

  def start(n) do
    spawn(fn() -> init(n) end)
  end

  def init(n) do
    {collector, sinks} = Collect.start(n)
    network = setup(n, sinks)
    Sorter.start(collector, network)
  end

  def setup(2, [s1, s2]) do
    cmp = comp(s1, s2)
    [cmp, cmp]
  end
  def setup(n, sinks) do
    n = div(n,2)
    {sink_low, sink_high} = Enum.split(sinks, n)
    merge_low = merge(n, sink_low)
    merge_high = merge(n, sink_high)
    {cross_low, cross_high} = cross(merge_low, merge_high)
    in_low = setup(n, cross_low)
    in_high = setup(n, cross_high)
    in_low ++ in_high
  end
  
  def merge(2, [s1,s2]) do
    cmp1 = comp(s1,s2)
    [cmp1, cmp1]
  end
  def merge(n, sinks) do
    n = div(n,2)
    {sink_low, sink_high} = Enum.split(sinks, n)
    merged_low =  merge(n, sink_low)    
    merged_high =  merge(n, sink_high)
    zipced = zipc(merged_low, merged_high)
    zipced ++ zipced
  end  

  def zipc([], []) do [] end
  def zipc([l|low], [h|high]) do
    cmp = comp(l,h)
    [cmp | zipc(low, high)]
  end
   

  
  def cross(low, high) do
    cross(low,  Enum.reverse(high), [])
  end

  def cross([], [], crossed) do
    {Enum.reverse(crossed), crossed}
  end
  def cross([l|low], [h|high], crossed) do
    cmp = comp(l, h)
    cross(low, high, [cmp | crossed])
  end
  
  
  def comp(low, high) do spawn(fn() -> comp(0, low, high) end)  end
  
  def comp(n, low, high) do
    receive do
      {:done, ^n} ->
	send(low, {:done, n})   
	send(high, {:done, n})
      {:epoc, ^n, x1} ->
	receive do
          {:epoc, ^n, x2} ->
            if x1 < x2 do
              send(low,  {:epoc, n, x1})
              send(high, {:epoc, n, x2})
            else
              send(low, {:epoc, n, x2})
              send(high,{:epoc, n, x1})
            end
            comp(n+1, low, high)
	end
    end
  end
  
end
