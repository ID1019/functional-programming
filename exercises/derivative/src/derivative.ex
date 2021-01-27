defmodule Derivative do

  @type literal() ::  {:num, number()}  | {:var, atom()}
  @type expr() :: {:exp, literal(), {:num, number()}}
  | {:add, expr(), expr()}
  | {:mul, expr(), expr()}
  | literal()

  @spec deriv(expr(), atom()) :: expr()

  def test() do
    test =
      {:add, {:mul, {:num, 4}, {:exp, {:var, :x}, {:num, 2}}},
       {:add, {:mul, {:num, 3}, {:var, :x}}, {:num, 42}}}

    der = deriv(test, :x)
    simpl = simplify(der)
    printp(test)
    IO.write("\n")
    printp(der)
    IO.write("\n")
    printp(simpl)
    IO.write("\n")
  end

  def deriv({:num, _}, _) do {:num, 0} end
  def deriv({:var, v}, v) do {:num, 1} end
  def deriv({:var, _}, _) do {:num, 0} end
  def deriv({:mul, e1, e2}, v) do
    {:add, {:mul, deriv(e1, v), e2}, {:mul, e1, deriv(e2, v)}}
  end
  def deriv({:exp, {:var, v}, {:num, c}}, v) do
    {:mul, {:num, c}, {:exp, {:var, v}, {:num, c - 1}}}
  end
  def deriv({:add, e1, e2}, v) do
    {:add, deriv(e1, v), deriv(e2, v)}
  end

  
  def simplify({:num, c}) do {:num, c} end
  def simplify({:var, c}) do {:var, c} end
  def simplify({:exp, e1, e2}) do
    simplify_exp(simplify(e1), simplify(e2))
  end
  def simplify({:mul, e1, e2}) do
    simplify_mul(simplify(e1), simplify(e2))
  end
  def simplify({:add, e1, e2}) do
    simplify_add(simplify(e1), simplify(e2))
  end  

  def simplify_exp({:num, 0}, _) do {:num, 1} end
  def simplify_exp(_, {:num, 0}) do {:num, 1} end
  def simplify_exp({:num, 1}, e2) do e2 end
  def simplify_exp(e1, {:num, 1}) do e1 end      
  def simplify_exp(e1, e2) do       
    {:exp, e1, e2}
  end

  def simplify_add({:num, 0}, e2) do e2 end
  def simplify_add(e1, {:num, 0}) do e1 end
  def simplify_add(e1, e2) do       
    {:add, e1, e2}
  end

  def simplify_mul({:num, 0}, _) do {:num, 0} end
  def simplify_mul(_, {:num, 0}) do {:num, 0} end
  def simplify_mul({:num, 1}, e2) do e2 end
  def simplify_mul(e1, {:num, 1}) do e1 end
  def simplify_mul({:num, c1}, {:num, c2}) do       
    {:num, c1+c2}
  end    
  def simplify_mul({:num, c1}, {:mul, {:num, c2}, e2}) do       
    {:mul, {:num, c1+c2}, e2}
  end      
  def simplify_mul({:mul, {:num, c1}, e1}, {:num, c2}) do       
    {:mul, {:num, c1+c2}, e1}
  end 
  def simplify_mul(e1, e2) do       
    {:mul, e1, e2}
  end    

  
  def printp({:num, c}) do IO.write("#{c}") end
  def printp({:var, v}) do IO.write("#{v}") end
  def printp({:exp, e1, e2}) do
    printp(e1)
    IO.write("^")
    printp(e2)
  end
  def printp({:mul, e1, e2}) do
    printp(e1)
    IO.write(" * ")
    printp(e2)
  end
  def printp({:add, e1, e2}) do
    printp(e1)
    IO.write(" + ")
    printp(e2)
  end

  def printpp({:num, c}) do IO.write("#{c}") end
  def printpp({:var, v}) do IO.write("#{v}") end
  def printpp({:exp, e1, e2}) do
    printpp(e1)
    IO.write("^")
    printpp(e2)
  end
  def printpp({:mul, e1, e2}) do
    printpp(e1)
    IO.write(" * ")
    printpp(e2)
  end
  def printpp({:add, e1, e2}) do
    IO.write("(")
    printpp(e1)
    IO.write(" + ")
    printpp(e2)
    IO.write(")")
  end
end
