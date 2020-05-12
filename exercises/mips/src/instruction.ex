defmodule Instruction do

  def start(code, reg, alu, nxt, ctrl) do
    spawn_link(fn() ->  init(code, reg, alu, nxt, ctrl) end)
  end

  def init(code, reg, alu, nxt, ctrl) do
    instruction(code, reg, alu, nxt, ctrl)
  end  

  def instruction(code, reg, alu, nxt, ctrl) do
    receive do
      {:nxt, :halt} ->
	:ok

      {:nxt, pc} ->
	instr = Program.read(code, pc)

	# no decodeing is done, all instructions are equal (shamt is ignored for the tie being, only used in shift operations)
	<<op::6, rs::5, rt::5, rest::binary>>  = instr
	<<rd::5, _::5, fnct::6>> = rest
	<<imm::16>> = rest

	:io.format("ins: ~w ~n", [op])
	
	# funct is sent through the cntrl process rather than to a separate alu-controler
	send(reg,  {:instr, rs, rt, rd})	    
	send(ctrl, {:instr, op, fnct})
	send(alu,  {:instr, imm})
	send(nxt,  {:instr, pc+4, imm})

	instruction(code, reg, alu, nxt, ctrl)
    end
  end
  

end
