defmodule CPU do
  
  def start(code, data, out) do
    mem = Memory.start(data)
    brn = Branch.start()
    alu = ALU.start(mem, brn, out)
    reg = Register.start(alu, mem)
    ctrl = Controller.start(reg, alu, mem, brn)
    instr = Instruction.start(code, reg, alu, brn, ctrl)

    send(mem, {:init, reg})
    send(brn, {:init, instr})
  end
  
  
end
