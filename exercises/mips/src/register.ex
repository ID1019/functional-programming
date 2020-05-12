defmodule Register do


  def start(alu, mem) do
    spawn_link(fn() -> init(alu, mem) end)
  end

  def init(alu, mem) do
    reg = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    register(reg, alu, mem)
  end
  

  def register(reg, alu, mem) do
    receive do

      {:instr, :halt} ->
	:io.format("reg: halt~n", [])
	send(alu, {:reg, :halt})
	:ok

      {:instr, rs, rt, rd} ->
	a = read(reg, rs)
	b = read(reg, rt)
	send(alu, {:reg, a, b})
	send(mem, {:reg, b})
	:io.format("reg: a ~w, b ~w~n", [a,b])
	receive do
	  {:ctrl, op} ->
	    receive do
	      {:mem, val} ->
		:io.format("reg: val ~w~n", [val])
		register(update(reg, op, rt, rd, val), alu, mem)
	    end
	end
    end
  end

  def update(reg, :noop, _, _, _) do reg end
  def update(reg, :wr_rt, rt, _, val) do write(reg, rt, val) end
  def update(reg, :wr_rd, _, rd, val) do put_elem(reg, rd, val) end
  
  def read(  _, 0) do 0 end  
  def read(reg, i) do elem(reg, i) end

  def write(reg, 0, _) do reg end
  def write(reg, i, val) do put_elem(reg, i, val) end  
		
		
	
	

  
end
