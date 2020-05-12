defmodule Next do


  def start() do
    spawn_link(fn() -> init() end)
  end

  def init() do
    receive do
      {:init, instr} ->
	next(instr)
    end
  end
  
  def next(instr) do
    receive do
      {:instr, pc, imm} ->
	:io.format("nxt: ~w ~w~n", [pc, imm])
	receive do
	  {:alu, zero} ->
	    :io.format("nxt: zero ~w~n", [zero])
	    receive do 
	      {:ctrl, :halt} ->
		send(instr, {:nxt, :halt})
		:ok
	      {:ctrl, brn} ->
		:io.format("nxt: brn ~w~n", [brn])
		send(instr, {:nxt, branch(pc, imm, brn, zero)})
		next(instr)
	    end
	end
    end
  end

  def branch(pc, imm, brn, zero) do
    if (brn and zero) do imm*4 else pc end
  end
  
  
end
