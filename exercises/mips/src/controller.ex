defmodule Controller do

  def start(reg, alu, mem, nxt) do
    spawn_link(fn() -> init(reg, alu, mem, nxt) end)
  end

  def init(reg, alu, mem, nxt) do
    controller(reg, alu, mem, nxt)
  end
  
  def controller(reg, alu, mem, nxt) do
    receive do

      # ALU operations

      {:instr, 0, fnct} ->
	:io.format("ctr: alu ~w~n", [fnct])
	send(reg, {:ctrl, :wr_rd})
	send(alu, {:ctrl, fnct, :reg})
	send(mem, {:ctrl, :noop})
	send(nxt, {:ctrl, false})
	controller(reg, alu, mem, nxt)

      # branch
      {:instr, 4, _} ->
	:io.format("ctr: branch~n", [])
	send(reg, {:ctrl, :noop})
	send(alu, {:ctrl, 34, :reg})
	send(mem, {:ctrl, :noop})
	send(nxt, {:ctrl, true})

      # addi 
      {:instr, 8, _} ->
	:io.format("ctr: addi~n", [])
	send(reg, {:ctrl, :wr_rt})
	send(alu, {:ctrl, 32, :imm})
	send(mem, {:ctrl, :noop})
	send(nxt, {:ctrl, false})
	controller(reg, alu, mem, nxt)
	
	# out
      {:instr, 13, _} ->
	:io.format("ctr: out~n", [])
	send(reg, {:ctrl, :noop})
	send(alu, {:ctrl, :out})
	send(mem, {:ctrl, :noop})
	send(nxt, {:ctrl, false})
	controller(reg, alu, mem, nxt)

      # load 
      {:instr, 35, _} ->
	:io.format("ctr: load~n", [])
	send(reg, {:ctrl, :wr_rt})
	send(alu, {:ctrl, 32, :imm})
	send(mem, {:ctrl, :read})
	send(nxt, {:ctrl, false})
	controller(reg, alu, mem, nxt)

      #store
      {:instr, 43, _} ->

	send(reg, {:ctrl, :noop})
	send(alu, {:ctrl, 32, :imm})
	send(mem, {:ctrl, :write})
	send(nxt, {:ctrl, false})
	controller(reg, alu, mem, nxt)	

      # halt
      {:instr, 63, _} ->
	send(reg, {:ctrl, :halt})
	send(alu, {:ctrl, :halt})
	send(mem, {:ctrl, :halt})
	send(nxt, {:ctrl, :halt})
	:ok

      strange ->
	:io.format("ctr: strange ~w~n", [strange])
	:ok

  end
  end
    

  
end
