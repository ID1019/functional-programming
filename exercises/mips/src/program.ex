defmodule Program do


  def assemble(prgm) do
    {:code, List.to_tuple(Enum.map(prgm, fn(instr) -> encode(instr) end))}
  end

  def read({:code, code}, pc) do
    # program counter is per byte
    elem(code, div(pc, 4))
  end

  def encode(instr) do
    case instr do
      ## R-type 
      {:add, rd, rs, rt}  -> << 0::6, rs::5, rt::5, rd::5, 0::5, 32::6>>
      {:sub, rd, rs, rt}  -> << 0::6, rs::5, rt::5, rd::5, 0::5, 34::6>>

      ## Load, store, 
      {:addi, rt, rs, imm} -> << 8::6, rs::5, rt::5, imm::16>>

      {:lw,   rt, rs, imm} -> <<35::6, rs::5, rt::5, imm::16>>
      {:sw,   rt, rs, imm} -> <<43::6, rs::5, rt::5, imm::16>>

      ## Branch 
      {:beq, rs, rt, imm} -> <<4::6, rs::5, rt::5, imm::16>>
      {:bne, rs, rt, imm} -> <<5::6, rs::5, rt::5, imm::16>>			   

      ## extra instructions 
      {:out, rs}          -> <<13::6, rs::5, 0::5, 0::16>>
      :halt               -> <<63::6, 0::5, 0::5, 0::16>>	
    end
  end
  

end
