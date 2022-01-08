defmodule ALU do

  @add 32
  @sub 34
  @xor 38

  def start(mem, brn, out) do
    spawn_link(fn() -> init(mem, brn, out) end)
  end

  def init(mem, brn, out) do
    alu(mem, brn, out)
  end

  def alu(mem, brn, out) do
    receive do
      {:reg, vs, vt} ->
	receive do
	  {:instr, imm} ->
	    receive do
	      {:ctrl, :halt} ->
		:io.format("alu: halt~n", [])		
		send(out, {:alu, :done})
		send(mem, {:alu, 0})		
		send(brn, {:alu, false})
		:ok

	      {:ctrl, :out} ->
		:io.format("alu: out ~w~n", [vs])		
		send(out, {:alu, vs})
		send(mem, {:alu, 0})
		send(brn, {:alu, false})
		alu(mem, brn, out)
		
	      {:ctrl, fnct, src} ->
		val = case src do
		      :imm -> imm
		      :reg -> vt
		    end
		res = op(fnct, vs, val)
		:io.format("alu: fnct ~w, src ~w, vs ~w, val ~w, res ~w~n", [fnct, src, vs, val, res])		
		send(mem, {:alu, res})
		send(brn, {:alu, res == 0})
		alu(mem, brn, out)
	    end
	end
    end
  end

  ## The result should be scaled to 32 bits 
  def op(@add, a, b) do a+b end
  def op(@sub, a, b) do a-b end
  def op(@xor, a, b) do Bitwise.bxor(a, b) end  

end
