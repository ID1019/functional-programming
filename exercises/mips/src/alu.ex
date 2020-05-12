defmodule ALU do


  def start(mem, nxt, out) do
    spawn_link(fn() -> init(mem, nxt, out) end)
  end

  def init(mem, nxt, out) do
    alu(mem, nxt, out)
  end

  def alu(mem, nxt, out) do
    receive do
      {:reg, a, b} ->
	receive do
	  {:instr, imm} ->
	    receive do
	      {:ctrl, :halt} ->

		:io.format("alu: halt~n", [])		
		send(out, {:alu, :done})
		send(mem, {:alu, 0})		
		send(nxt, {:alu, false})
		:ok

	      {:ctrl, :out} ->
		:io.format("alu: out~n", [])		
		send(out, {:alu, a})
		send(mem, {:alu, 0})
		send(nxt, {:alu, false})
		alu(mem, nxt, out)
		
	      {:ctrl, fnct, src} ->
		:io.format("alu: ~w ~w~n", [fnct, src])		
		b = case src do
		      :imm -> imm
		      :reg -> b
		    end
		res = op(fnct, a, b)
		send(mem, {:alu, res})
		send(nxt, {:alu, if res == 0 do  true else false end})
		alu(mem, nxt, out)
	    end
	end
    end
  end

  ## The result should be scaled to 32 bits 
  def op(32, a, b) do a + b end
  def op(34, a, b) do a - b end
  

end
