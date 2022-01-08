defmodule Memory do

  def start(data) do
    spawn_link(fn() -> init(data) end)
  end

  def init(data) do
    receive do
      {:init, reg} ->
	memory(data, reg)
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
		:io.format("mem: halt~n", [])
		send(reg, {:mem, 0})		
		:ok
	      {:ctrl, rw} ->
		{mem, val} = rw(mem, addr, val, rw)
		send(reg, {:mem, val})
		memory(mem, reg)
	    end
	end
    end
  end
  
  def rw(mem, addr, val, instr) do
    case instr do 

      :rword ->
	val = Program.read_word(mem, addr)
	:io.format("mem: read word ~w from ~w~n", [val, addr])
	{mem, val}

      :wword ->
	:io.format("mem: store word ~w at ~w~n", [val, addr])
	## Convert from integer to binary. 
	mem = Program.write_word(mem, addr, val)
	{mem, 0}

      :rbyte ->
	val = Program.read_byte(mem, addr)
	:io.format("mem: read byte ~w from ~w~n", [val, addr])
	{mem, val}

      :wbyte ->
	:io.format("mem: store byte ~w at ~w~n", [val, addr])
	mem = Program.write_byte(mem, addr, val)
	{mem, 0}

      :frw ->
	:io.format("mem: frw ~w~n", [addr])
	{mem, addr}
    end
  end


end
