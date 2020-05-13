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
      {:add, dst, a, b}  -> << 0::6, a::5, b::5, dst::5, 0::5, 32::6>>
      {:sub, dst, a, b}  -> << 0::6, a::5, b::5, dst::5, 0::5, 34::6>>

      ## Load, store, 
      {:addi, dst, a, imm} -> << 8::6, a::5, dst::5, imm::16>>

      {:lw,   src, a, imm} -> <<35::6, a::5, src::5, imm::16>>
      {:sw,   dst, a, imm} -> <<43::6, a::5, dst::5, imm::16>>

      ## Branch 
      {:beq, a, b, imm} -> <<4::6, a::5, b::5, imm::16>>
      {:bne, a, b, imm} -> <<5::6, a::5, b::5, imm::16>>			   

      ## extra instructions 
      {:out, a}          -> <<13::6, a::5, 0::5, 0::16>>
      {:halt}            -> <<63::6, 0::5, 0::5, 0::16>>	
    end
  end
  

end
