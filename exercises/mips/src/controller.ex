defmodule Controller do

  def start(reg, alu, mem, brn) do
    spawn_link(fn() -> init(reg, alu, mem, brn) end)
  end

  def init(reg, alu, mem, brn) do
    controller(reg, alu, mem, brn)
  end
  
  def controller(reg, alu, mem, brn) do
    receive do

      # ALU operations

      {:instr, 0, fnct} ->
	:io.format("ctr: alu ~w~n", [fnct])
	send(reg, {:ctrl, :wrd})
	send(alu, {:ctrl, fnct, :reg})
	send(mem, {:ctrl, :frw})
	send(brn, {:ctrl, :nbr})
	controller(reg, alu, mem, brn)

      # branch
      {:instr, 4, _} ->
	:io.format("ctr: beq~n", [])
	send(reg, {:ctrl, :noop})
	send(alu, {:ctrl, 34, :reg})  # sub
	send(mem, {:ctrl, :frw})
	send(brn, {:ctrl, :beq})
	controller(reg, alu, mem, brn)	

      {:instr, 5, _} ->
	:io.format("ctr: bne~n", [])
	send(reg, {:ctrl, :noop})
	send(alu, {:ctrl, 34, :reg})  # sub
	send(mem, {:ctrl, :frw})
	send(brn, {:ctrl, :bne})
	controller(reg, alu, mem, brn)
	
      # addi 
      {:instr, 8, _} ->
	:io.format("ctr: addi~n", [])
	send(reg, {:ctrl, :wr_rt})
	send(alu, {:ctrl, 32, :imm})
	send(mem, {:ctrl, :frw})
	send(brn, {:ctrl, :nbr})
	controller(reg, alu, mem, brn)
	
	# out
      {:instr, 13, _} ->
	:io.format("ctr: out~n", [])
	send(reg, {:ctrl, :nop})
	send(alu, {:ctrl, :out})
	send(mem, {:ctrl, :frw})
	send(brn, {:ctrl, :nbr})
	controller(reg, alu, mem, brn)

      # load 
      {:instr, 35, _} ->
	:io.format("ctr: load~n", [])
	send(reg, {:ctrl, :wrt})
	send(alu, {:ctrl, 32, :imm})
	send(mem, {:ctrl, :read})
	send(brn, {:ctrl, :nbr})
	controller(reg, alu, mem, brn)

      #store
      {:instr, 43, _} ->

	send(reg, {:ctrl, :nop})
	send(alu, {:ctrl, 32, :imm})
	send(mem, {:ctrl, :write})
	send(brn, {:ctrl, :nbr})
	controller(reg, alu, mem, brn)	

      # halt
      {:instr, 63, _} ->
	send(reg, {:ctrl, :halt})
	send(alu, {:ctrl, :halt})
	send(mem, {:ctrl, :halt})
	send(brn, {:ctrl, :halt})
	:ok

      strange ->
	:io.format("ctr: strange ~w~n", [strange])
	:ok

  end
  end
    

  
end
