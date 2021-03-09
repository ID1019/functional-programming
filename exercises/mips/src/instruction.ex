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
	:io.format("ins: halt~n", [])
	:ok

      {:brn, pc} ->
	instr = Program.read_instruction(code, pc)

	# no decodeing is done, all instructions are equal, shamt is
	# ignored for the time being, only used in shift operations
	<<op::6, rs::5, rt::5, rest::binary>>  = instr
	<<rd::5, _shamt::5, fnct::6>> = rest
	<<imm::integer-signed-16>>  = rest

	:io.format("ins: pc ~w, op ~w, imm ~w ~n", [pc, op, imm])
	

	## to the register unit
	send(reg,  {:instr, rs, rt, rd})	    

	# funct is sent through the cntrl process 
	send(ctrl, {:instr, op, fnct})

	# ALU will be send the immediate value directly
	send(alu,  {:instr, imm})

	## immediate value is sign extended and shiftet 
	send(brn,  {:instr, imm})

	instruction(code, reg, alu, brn, ctrl)
    end
  end
  

end
