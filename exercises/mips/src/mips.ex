defmodule MIPS do


  def run(prgm) do
    {code, data} = Program.load(prgm)

    :io.format("code : ~w\n\ndata: ~w\n\n", [code, data])
    
    out = Out.start()

    mem = Memory.start(data)

    brn = Branch.start()

    alu = ALU.start(mem, brn, out)

    reg = Register.start(alu, mem)

    ctrl = Controller.start(reg, alu, mem, brn)

    instr = Instruction.start(code, reg, alu, brn, ctrl)

    send(mem, {:init, reg})
    send(brn, {:init, instr})

    Out.wait(out)
  end

  
  
end
