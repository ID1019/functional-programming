defmodule Emulator do

  def run(code, mem) do
    reg = Register.new()
    run(0, code, mem, reg, [])
  end


  def run(pc, code, mem, reg, out) do

    next = Program.read(code, pc)

    case next do

      {:halt} ->
	Enum.reverse(out)

      {:out, rs} ->
	a = Register.read(reg, rs)
	run(pc+1, code, mem, reg, [a|out])
	
      {:add, rd, rs, rt} ->
	a = Register.read(reg, rs)
	b = Register.read(reg, rt)
	reg = Register.write(reg, rd, a + b)
	run(pc+1, code, mem, reg, out)

      {:sub, rd, rs, rt} ->
	a = Register.read(reg, rs)
	b = Register.read(reg, rt)
	reg = Register.write(reg, rd, a - b)
	run(pc+1, code, mem, reg, out)

      {:addi, rd, rs, imm} ->
	a = Register.read(reg, rs)
	reg = Register.write(reg, rd, a + imm)
	run(pc+1, code, mem, reg, out)

      {:beq, rs, rt, imm} ->
	a = Register.read(reg, rs)
	b = Register.read(reg, rt)
	pc = if a == b do  pc+imm else pc end
	run(pc+1, code, mem, reg, out)

      {:bne, rs, rt, imm} ->
	a = Register.read(reg, rs)
	b = Register.read(reg, rt)
	pc = if a != b do  pc+imm else pc end
	run(pc+1, code, mem, reg, out)

      {:lw, rd, rs, imm} ->
	a = Register.read(reg, rs)	
	addr = a + imm
	val = Memory.read(mem, addr)
	reg = Register.write(reg, rd, val)
	run(pc+1, code, mem, reg, out)
      
      {:sw, rs, rt, imm} ->
	vs = Register.read(reg, rs)
	vt = Register.read(reg, rt)		
	addr = vt + imm
	mem = Memory.write(mem, addr, vs)
	run(pc+1, code, mem, reg, out)
    end
  end



end
