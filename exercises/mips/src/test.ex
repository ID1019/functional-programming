defmodule Test do

  def test() do
    MIPS.run(demo())
  end

  ## R-instructions: {op, dest, src, target}
  ## I-instructions: {op, target, src, imm}
  ##    Except for beq and bne: {op, src, target, imm} 
  
  def demo() do
    {:prgm,
     [{:addi, 1, 0, 10},    # $1 <- 10 
      {:addi, 2, 0, 5},     # $2 <- 5 
      {:add, 3, 1, 2},      # $3 <- $1 + $2
      {:sw, 3, 0, 8},       # mem[0 + 8] <- $3
      {:lw, 4, 0, 8},       # $4 <- mem[0+8]
      {:addi, 5, 0, 1},     # $5 <- 1
      {:label, :loop},
      {:sub, 4, 4, 5},      # $4 <- $4 - $5
      {:out, 4},            # out $4
      {:bne, 4, 0, :loop},  # branch if not zero
      :halt],
     [{:space, 12}]}
    end

  def fib(n) do
    {:prgm,
     [{:addi, 1, 0, n},     # $1 <- n
      {:addi, 3, 0, 1},     # $3 <- 1     
      {:beq,  1, 0, :done}, # if n == 0 -> :done
      {:out, 3},
      {:addi, 1, 1,-1},     # $1 <- $1 - 1
      {:beq,  1, 0, :done}, # if n == 0 -> :done
      {:addi, 2, 0, 1},     # $2 <- 1

      {:label, :loop},
      {:addi, 4, 3, 0},     # $4 <- $3
      {:out, 3},
      {:add,  3, 2, 3},     # $3 <- $2 + $3
      {:addi, 1, 1,-1},     # $1 = $1 - 1      
      {:beq,  1, 0, :done}, # if n == 0 -> :done
      {:addi, 2, 4, 0},     # $2 <- $4
      {:beq,  0, 0, :loop}, # -> :done

      {:label, :done},
      {:out, 3},      
      :halt],
     []}
  end


  def hello() do
    {:prgm, 
     [{:ori, 1, 0, :hello},    # this is only 16 bits but it's ok 
      {:label, :loop},
      {:lb, 2, 1, 0},          # load byte $0 + $1 into $2
      {:beq, 2, 0, :done},     # if zero we're done
      {:out, 2},               # output the byte
      {:addi, 1, 1, 1},        # increment $1 
      {:beq, 0, 0, :loop},     # jump to :loop
      {:label, :done},         
      :halt                    # done
     ],
     [{:label, :hello},
      {:asciiz,'hello'}]
    }
  end
  
  


end
