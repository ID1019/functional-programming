defmodule Branch do


  def start() do
    spawn_link(fn() -> init() end)
  end

  def init() do
    receive do
      {:init, instr} ->
	## send the first pc value to the instruction unit 
	send(instr, {:brn, 0})
	branch(instr, 0)
    end
  end
  
  def branch(instr, pc) do
    receive do
      {:instr, imm} ->
	receive do
	  {:alu, zero} ->
	    receive do 
	      {:ctrl, :halt} ->
		send(instr, {:brn, :halt})
		:io.format("brn: halt~n", [])
		:ok
	      {:ctrl, branch} ->
		pc = next(branch, pc, imm, zero)
		send(instr, {:brn, pc})
		branch(instr, pc)		
	    end
	end
    end
  end

  def next(branch, pc, imm, zero) do
      case branch do
	:beq ->
	  :io.format("brn: beq ~w, pc ~w, imm ~w~n", [zero, pc, imm])
	  beq(pc, imm, zero)
	:bne ->
	  :io.format("brn: bne ~w, pc ~w, imm ~w~n", [zero, pc, imm])
	  beq(pc, imm, not(zero))
	:nbr ->
	  :io.format("brn: nbr ~n", [])
	  increment(pc)
      end
  end


  def beq(pc, imm, zero) do
    if zero do imm + pc + 4 else pc + 4 end 
  end

  def increment(pc) do 	pc + 4 end
  
  
end
