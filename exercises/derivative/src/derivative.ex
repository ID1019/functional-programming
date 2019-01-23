defmodule Derivative do

  @type constant() :: {:const, number()} | {:const, atom()}
  @type literal() :: constant() | {:var, atom()}
  @type expr() :: {:exp, constant(), literal(), integer()} | {:mul, constant(), literal()} | literal()

  @spec deriv(expr(), atom()) :: expr()

  def test() do
    test =
      {:add, {:mul, {:const, 4}, {:exp, {:var, :x}, {:const, 2}}},
       {:add, {:mul, {:const, 3}, {:var, :x}}, {:const, 42}}}

    der = deriv(test, :x)
    simpl = simplify(der)
    printp(test)
    IO.write("\n")
    printp(der)
    IO.write("\n")
    printp(simpl)
    IO.write("\n")
  end

  def deriv({:const, _}, _) do {:const, 0} end
  def deriv({:var, v}, v) do {:const, 1} end
  def deriv({:var, y}, _) do {:const, 0} end
  def deriv({:mul, e1, e2}, v) do
    {:add, {:mul, deriv(e1, v), e2}, {:mul, e1, deriv(e2, v)}}
  end
  def deriv({:exp, {:var, v}, {:const, c}}, v) do
    {:mul, {:const, c}, {:exp, {:var, v}, {:const, c - 1}}}
  end
  def deriv({:add, e1, e2}, v) do
    {:add, deriv(e1, v), deriv(e2, v)}
  end

  def simplify({:const, c}) do {:const, c} end
  def simplify({:var, c}) do {:var, c} end
  def simplify({:exp, e1, e2}) do
    case simplify(e2) do
      {:const, 0} ->
        {:const, 1}

      {:const, 1} ->
        simplify(e1)

      s2 ->
        case simplify(e1) do
          {:const, 0} ->
            {:const, 0}

          {:const, 1} ->
            {:const, 1}

          s1 ->
            {:exp, s1, s2}
        end
    end
  end
  def simplify({:mul, e1, e2}) do
    case simplify(e1) do
      {:const, 0} ->
        {:const, 0}

      {:const, 1} ->
        simplify(e2)

      s1 ->
        case simplify(e2) do
          {:const, 0} ->
            {:const, 0}

          {:const, 1} ->
            s1

          s2 ->
            {:mul, s1, s2}
        end
    end
  end
  def simplify({:add, e1, e2}) do
    case simplify(e1) do
      {:const, 0} ->
        simplify(e2)

      s1 ->
        case simplify(e2) do
          {:const, 0} ->
            s1

          s2 ->
            {:add, s1, s2}
        end
    end
  end

  def printp({:const, c}) do IO.write("#{c}") end
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

  def printpp({:const, c}) do IO.write("#{c}") end
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
