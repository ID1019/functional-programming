defmodule Instruction do

  def start(code, reg, alu, brn, ctrl) do
    spawn_link(fn() ->  init(code, reg, alu, brn, ctrl) end)
  end

  def init(code, reg, alu, brn, ctrl) do
    instruction(code, reg, alu, brn, ctrl)
  end  

  def instruction(code, reg, alu, brn, ctrl) do
    receive do
      {:brn, :halt} ->
	:ok

      {:brn, pc} ->
	instr = Program.read(code, pc)

	# no decodeing is done, all instructions are equal (shamt is ignored for the tie being, only used in shift operations)
	<<op::6, rs::5, rt::5, rest::binary>>  = instr
	<<rd::5, _::5, fnct::6>> = rest
	<<imm::size(2)-big-integer-signed-unit(8)>>  = rest

	:io.format("ins: pc ~w, op ~w, imm ~w ~n", [pc, op, imm])
	
	# funct is sent through the cntrl process rather than to a separate alu-controler
	send(reg,  {:instr, rs, rt, rd})	    
	send(ctrl, {:instr, op, fnct})
	send(alu,  {:instr, imm})
	send(brn,  {:instr, pc+4, imm})

	instruction(code, reg, alu, brn, ctrl)
    end
  end
  

end
