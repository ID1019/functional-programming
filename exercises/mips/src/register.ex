defmodule Register do


  def start(alu, mem) do
    spawn_link(fn() -> init(alu, mem) end)
  end

  def init(alu, mem) do
    reg = register()
    register(reg, alu, mem)
  end
  

  def register(reg, alu, mem) do
    :io.format("reg: ~w\n", [reg])
    receive do

      {:instr, rs, rt, rd} ->
	vs = read(reg, rs)
	vt = read(reg, rt)
	send(alu, {:reg, vs, vt})
	send(mem, {:reg, vt})
	:io.format("reg: vs ~w, vt ~w~n", [vs,vt])
	receive do
	  {:ctrl, :halt} ->
	    :io.format("reg: halt~n", [])	    
	    :ok
	  {:ctrl, op} ->
	    :io.format("reg: ctrl  ~w~n", [op])
	    receive do
	      {:mem, val} ->
		:io.format("reg: val ~w~n", [val])
		register(update(reg, op, rt, rd, val), alu, mem)
	    end
	end
    end
  end

  
  def update(reg, :nop, _, _, _) do reg end
  def update(reg, :wrt, rt, _, val) do write(reg, rt, val) end
  def update(reg, :wrd, _, rd, val) do put_elem(reg, rd, val) end
  

  
  def register() do
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
  end

  def read(  _, 0) do 0 end  
  def read(reg, i) do elem(reg, i) end

  def write(reg, 0, _) do reg end
  def write(reg, i, val) do put_elem(reg, i, val) end  
		
		
	
	

  
end
