defmodule Test do

  def test() do
    MIPS.run(demo())
  end

  def demo() do
    [{:addi, 1, 0, 10},
     {:addi, 2, 0, 5},
     {:add, 3, 1, 2},
     {:sw, 3, 0, 7},
     {:lw, 4, 0, 7},     
     {:out, 4},
     {:halt}]
  end
  
  

end
