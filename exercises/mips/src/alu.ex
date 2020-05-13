defmodule ALU do


  def start(mem, brn, out) do
    spawn_link(fn() -> init(mem, brn, out) end)
  end

  def init(mem, brn, out) do
    alu(mem, brn, out)
  end

  def alu(mem, brn, out) do
    receive do
      {:reg, a, b} ->
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
		:io.format("alu: out ~w~n", [a])		
		send(out, {:alu, a})
		send(mem, {:alu, 0})
		send(brn, {:alu, false})
		alu(mem, brn, out)
		
	      {:ctrl, fnct, src} ->
		b = case src do
		      :imm -> imm
		      :reg -> b
		    end
		res = op(fnct, a, b)
		:io.format("alu: fnct ~w, src ~w, a ~w, b ~w, res ~w~n", [fnct, src, a, b, res])		
		send(mem, {:alu, res})
		send(brn, {:alu, if res == 0 do  true else false end})
		alu(mem, brn, out)
	    end
	end
    end
  end

  ## The result should be scaled to 32 bits 
  def op(32, a, b) do a + b end
  def op(34, a, b) do a - b end
  def op(38, a, b) do Bitwise.bxor(a, b) end  

end
