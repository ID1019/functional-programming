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
		data = Data.read(mem, addr)
		:io.format("mem: read ~w from ~w~n", [data, addr])
		send(reg, {:mem, data})
		memory(mem, reg)

	      {:ctrl, :write} ->
		:io.format("mem: store ~w at ~w~n", [val, addr])
		mem = Data.write(mem, addr, val)
		send(reg, {:mem, 0})
		memory(mem, reg)

	      {:ctrl, :noop} ->
		send(reg, {:mem, addr})
		memory(mem, reg)
	    end
	end
    end
  end
  

end
