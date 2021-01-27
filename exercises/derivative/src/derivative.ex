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

    IO.write("expression: #{pprint(test)}\n")
    IO.write("derivative: #{pprint(der)}\n")
    IO.write("simplified: #{pprint(simpl)}\n")
  end

  def deriv({:num, _}, _) do {:num, 0} end
  def deriv({:var, v}, v) do {:num, 1} end
  def deriv({:var, _}, _) do {:num, 0} end
  def deriv({:add, e1, e2}, v) do
    {:add, deriv(e1, v), deriv(e2, v)}
  end
  def deriv({:mul, e1, e2}, v) do
    {:add, {:mul, deriv(e1, v), e2}, {:mul, e1, deriv(e2, v)}}
  end
  def deriv({:exp, e1, {:num, c}}, v) do
    {:mul,
     {:mul, {:num, c}, {:exp, e1, {:num, c - 1}}},
     deriv(e1, v)}
  end


  def simplify({:num, n}) do {:num, n} end
  def simplify({:var, v}) do {:var, v} end
  def simplify({:add, e1, e2}) do
    simplify_add(simplify(e1), simplify(e2))
  end
  def simplify({:mul, e1, e2}) do
    simplify_mul(simplify(e1), simplify(e2))
  end
  def simplify({:exp, e1, e2}) do
    simplify_exp(simplify(e1), simplify(e2))
  end
  

  def simplify_add({:num, 0}, e2) do e2 end  
  def simplify_add({:num, n1}, {:num, n2}) do {:num, n1+n2} end
  
  def simplify_add({:var, v}, {:var, v}) do {:mul, {:num, 2}, {:var, v}} end
  def simplify_add(e1, e2) do {:add, e1, e2} end

  def simplify_mul({:num, n1}, {:num, n2}) do {:num, n1*n2} end
  def simplify_mul({:num, 0}, _) do {:num, 0} end
  def simplify_mul(_, {:num, 0}) do {:num, 0} end    
  def simplify_mul({:num, 1}, e2) do e2 end
  def simplify_mul(e1, {:num, 1}) do e1 end    
  def simplify_mul({:var, v}, {:var, v}) do {:exp, {:var, v}, {:num, 2}} end
  def simplify_mul({:var, v}, {:exp, {:var, v}, {:num, n}}) do {:exp, {:var, v}, {:num, n+1}} end  
  def simplify_mul({:exp, {:var, v}, {:num, n}}, {:var, v}) do {:exp, {:var, v}, {:num, n+1}} end
  
  def simplify_mul({:num, n1}, {:mul, {:num, n2}, e2}) do
    {:mul, {:num, n1*n2}, e2}
  end
  def simplify_mul({:num, n1}, {:mul, e2, {:num, n2}}) do
    {:mul, {:num, n1*n2}, e2}
  end
  def simplify_mul({:mul, {:num, n1}, e1}, {:num, n2}) do
    {:mul, {:num, n1*n2}, e1}
  end
  def simplify_mul({:mul, e1, {:num, n1}}, {:num, n2}) do
    {:mul, {:num, n1*n2}, e1}
  end

  
  def simplify_mul(e1, e2) do {:mul, e1, e2} end  

  def simplify_exp(_,{:num, 0}) do  1 end  
  def simplify_exp(e1,{:num, 1}) do  e1 end
  def simplify_exp(e1,e2) do
    {:exp, e1, e2}
  end

  def pprint({:num, n}) do "#{n}" end
  def pprint({:var, v}) do "#{v}" end
  def pprint({:add, e1, e2}) do
    "#{pprint(e1)} + #{pprint(e2)}"
  end
  def pprint({:mul, e1, e2}) do
    "( #{pprint(e1)} * #{pprint(e2)} )"
  end  
  def pprint({:exp, e1, e2}) do
    "#{pprint(e1)}^#{pprint(e2)}"
  end

end

  
