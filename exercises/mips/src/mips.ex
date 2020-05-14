defmodule MIPS do


  def run(prgm) do
    code = Program.assemble(prgm)
    mem = Memory.new()
    out = Out.start()
    CPU.start(code, mem, out)
    Out.wait(out)
  end

  
  
end
