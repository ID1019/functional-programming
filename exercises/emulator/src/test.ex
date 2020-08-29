defmodule Test do

  def test() do
    code = Program.assemble(demo())
    mem = Memory.new([])
    out = Out.new()
    Emulator.run(code, mem, out)
  end

  def demo() do
    [{:addi, 1, 0, 10},    # $1 <- 10 
     {:addi, 2, 0, 5},     # $2 <- 5 
     {:add, 3, 1, 2},      # $3 <- $1 + $2
     {:sw, 3, 0, 7},       # mem[0 + 7] <- $3
     {:lw, 4, 0, 7},       # $4 <- mem[0+7]
     {:addi, 5, 0, 1},     # $5 <- 1
     {:sub, 4, 4, 5},      # $4 <- $4 - $5
     {:out, 4},            # out $4
     {:bne, 4, 0, -3},     # branch if not equal
     {:halt}]
  end
  
  

end
