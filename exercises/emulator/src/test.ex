defmodule Test do

  def test() do
    test(demo())
  end

  def test(prgm) do
    {code, data} = Program.load(prgm)
    out = Out.new()
    Emulator.run(code, data, out)
  end   

  def demo() do
    {:prgm,
	[{:addi, 1, 0, 5},    # $1 <- 5 
         {:lw, 2, 0, :arg},    # $2 <- data[:arg]
	 {:add, 4, 2, 1},      # $4 <- $2 + $1
	 {:addi, 5, 0, 1},     # $5 <- 1
	 {:label, :loop},
	 {:sub, 4, 4, 5},      # $4 <- $4 - $5
	 {:out, 4},            # out $4
	 {:bne, 4, 0, :loop},  # branch if not equal
	 :halt],
      [{:label, :arg},
       {:word, 7}]}
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


  

  

end
