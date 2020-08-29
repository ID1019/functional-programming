defmodule Memory do

  def start(mem) do
    spawn_link(fn() -> init(mem) end)
  end

  def init(mem) do
    receive do
      {:init, reg} ->
	memory(mem, reg)
    end
  end

  def memory(mem, reg) do
    receive do

      {:alu, addr} ->
	:io.format("mem: adr ~w~n", [addr])
	receive do
	  {:reg, val} ->
	    :io.format("mem: val ~w~n", [val])
	    receive do
	      {:ctrl, :halt} ->
		:ok

	      {:ctrl, :read} ->
		data = read(mem, addr)
		:io.format("mem: read ~w from ~w~n", [data, addr])
		send(reg, {:mem, data})
		memory(mem, reg)

	      {:ctrl, :write} ->
		:io.format("mem: store ~w at ~w~n", [val, addr])
		mem = write(mem, addr, val)
		send(reg, {:mem, 0})
		memory(mem, reg)

	      {:ctrl, :frw} ->
		send(reg, {:mem, addr})
		memory(mem, reg)
	    end
	end
    end
  end


  def new() do
    new([])
  end    

  def new(segments) do
    f = fn({start, data}, layout) ->
      last = start +  length(data) -1      
      Enum.zip(start..last, data) ++ layout
    end
    layout = List.foldr(segments, [], f)
    {:data, Map.new(layout)}
  end

  def read({:data, data}, i) do
    Map.get(data, i)
  end

  def write({:data, data}, i, val) do
    {:data, Map.put(data, i, val)}
  end  

end
