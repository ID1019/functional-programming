defmodule Emulator do

  def run(code, data, out) do
    reg = Register.new()
    run(0, code, data, reg, out)
  end


  def run(pc, code, data, reg, out) do

    next = Program.read_instruction(code, pc)

    :io.format("instruction ~w\n", [next])
    
    case next do

      :halt ->
	Out.close(out)

      {:out, rs} ->
	a = Register.read(reg, rs)
	:io.format("out:  ~w\n", [a])
	run(pc+4, code, data, reg, Out.put(out,a))
	
      {:add, rd, rs, rt} ->
	a = Register.read(reg, rs)
	b = Register.read(reg, rt)
	reg = Register.write(reg, rd, a + b)  # we're not handling overflow
	run(pc+4, code, data, reg, out)

      {:sub, rd, rs, rt} ->
	a = Register.read(reg, rs)
	b = Register.read(reg, rt)
	reg = Register.write(reg, rd, a - b)
	run(pc+4, code, data, reg, out)

      {:addi, rd, rs, imm} ->
	a = Register.read(reg, rs)
	reg = Register.write(reg, rd, a + imm)
	run(pc+4, code, data, reg, out)

      {:beq, rs, rt, imm} ->
	a = Register.read(reg, rs)
	b = Register.read(reg, rt)
	pc = if a == b do  pc+imm else pc end
	run(pc+4, code, data, reg, out)

      {:bne, rs, rt, imm} ->
	a = Register.read(reg, rs)
	b = Register.read(reg, rt)
	pc = if a != b do  pc+imm else pc end
	run(pc+4, code, data, reg, out)

      {:lw, rd, rs, imm} ->
	a = Register.read(reg, rs)	
	addr = a + imm
	val = Program.read_word(data, addr)
	reg = Register.write(reg, rd, val)
	run(pc+4, code, data, reg, out)
      
      {:sw, rs, rt, imm} ->
	vs = Register.read(reg, rs)
	vt = Register.read(reg, rt)		
	addr = vt + imm
	data = Program.write_word(data, addr, vs)
	run(pc+4, code, data, reg, out)

    end
  end



end
