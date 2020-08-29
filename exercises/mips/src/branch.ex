defmodule Branch do


  def start() do
    spawn_link(fn() -> init() end)
  end

  def init() do
    receive do
      {:init, instr} ->
	## send the first pc value
	send(instr, {:brn, 0})
	branch(instr)
    end
  end
  
  def branch(instr) do
    receive do
      {:instr, pc, imm} ->
	receive do
	  {:alu, zero} ->
	    receive do 
	      {:ctrl, :halt} ->
		send(instr, {:brn, :halt})
		:ok
	      {:ctrl, :beq} ->
		:io.format("brn: beq ~w, pc ~w, imm ~w~n", [zero, pc, imm])
		send(instr, {:brn, beq(pc, imm, zero)})
		branch(instr)
	      {:ctrl, :bne} ->
		:io.format("brn: bne ~w, pc ~w, imm ~w~n", [zero, pc, imm])
		send(instr, {:brn, beq(pc, imm, not(zero))})
		branch(instr)
	      {:ctrl, :nbr} ->
		:io.format("brn: nbr ~n", [])
		send(instr, {:brn, pc})
		branch(instr)
	    end
	end
    end
  end

  def beq(pc, imm, zero) do
    if zero do imm*4 + pc else pc end
  end
  
  
end
