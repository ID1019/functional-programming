defmodule Collect do

  def start(n) do
    collector = spawn(fn() -> init(n) end)
    sinks = setup(n, collector)
    {collector, sinks}
  end

  def init(n) do
    collector(0, n)
  end

  def setup(0, _) do
    []
  end
  def setup(i, collector) do
    [spawn(fn() -> output(0, i,collector) end) | setup(i-1, collector)]
  end

  def output(e, i, collector) do
    receive do
      {:epoch, ^e, val} ->
	send(collector, {:sorted, e, i, val})
	output(e+1, i, collector)
      {:done, ^e} ->
	:ok
    end
  end

  def collector(e, n) do
    receive do
      {:sort, ^e, ref, from} ->
	sorted = collect(e, n, [])
	send(from, {:sorted, ref, sorted})
	collector(e+1, n)
      {:done, ^e} ->
	:ok
    end
  end
  
  def collect(_, 0, sorted) do
    sorted
  end
  def collect(e, i, sofar) do
    receive do
      {:sorted, ^e, ^i, val} ->
	collect(e, i-1,[val|sofar])
    end
  end

end
