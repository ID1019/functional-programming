defmodule Sorter do

  def start(network, collector) do
    spawn(fn() -> init(network, collector) end)
  end

  def init(network, collector) do
    sorter(0, network, collector)
  end
  
  def sorter(epoch, network, collector) do
    receive do
      {:sort, this, ref, from} ->
	send(collector, {:sort, epoch, ref, from})
	Enum.each(Enum.zip(network, this), 
          fn({cmp, x}) -> send(cmp,{:epoc, epoch, x}) end)
	sorter(epoch+1, network, collector)
      :done ->
	send(collector, {:done, epoch})
	Enum.each(network, fn(cmp) -> send(cmp, {:done, epoch}) end)
    end
  end
  
end
