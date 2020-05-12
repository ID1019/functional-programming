defmodule CPU do
  
  def start(code, data, out) do
    mem = Memory.start(data)
    nxt = Next.start()
    alu = ALU.start(mem, nxt, out)
    reg = Register.start(alu, mem)
    ctrl = Controller.start(reg, alu, mem, nxt)
    instr = Instruction.start(code, reg, alu, nxt, ctrl)

    send(mem, {:init, reg})
    send(nxt, {:init, instr})
    send(instr, {:nxt, 0})
  end
  
  
end
