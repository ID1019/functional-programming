defmodule Test do

  def test(n) do
    # The default is the eager module.
    test(Eager, n)
  end

  def test(module, 1) do
    seq = [{:atm, :a}]
    IO.write("atom: \n    :a  \nshould result in {:ok, :a}\n")
    apply(module, :eval, [seq])
  end

  def test(module, 2) do
    seq = [{:match, {:var, :x}, {:atm, :a}}, {:cons, {:var, :x}, {:atm, :b}}]
    IO.write("sequence:\n  x = :a; {x,:b}\nshould result in {:ok, {:a,:b}}\n")
    apply(module, :eval, [seq])
  end

  def test(module, 3) do
    seq = [
      {:match, {:var, :x}, {:atm, :a}},
      {:match, {:var, :y}, {:cons, {:var, :x}, {:atm, :b}}},
      {:match, {:cons, :ignore, {:var, :z}}, {:var, :y}},
      {:var, :z}
    ]
    IO.write("sequence:\n x = :a; y = {x,:b};  {_,z} = y; z\n   should result in {:ok, :b}\n")
    apply(module, :eval, [seq])
  end

  def test(module, 4) do
    seq = [
      {:match, {:var, :x}, {:atm, :a}},
      {:case, {:var, :x},
       [{:clause, {:atm, :b}, [{:atm, :ops}]}, {:clause, {:atm, :a}, [{:atm, :yes}]}]}
    ]
    IO.write("case expression: \n x = :a; case x do :b -> :ops; :a -> :yes end \nshould result in {:ok, :yes}\n")
    apply(module, :eval, [seq])
  end

  def test(module, 5) do
    seq = [
      {:match, {:var, :x}, {:cons, {:atm, :a}, {:atm, :nil}}},
      {:case, {:var, :x},
       [
         {:clause, {:atm, :nil}, [{:atm, :ops}]},
         {:clause, {:cons, {:var, :hd}, {:var, :tl}}, [{:var, :hd}]}
       ]}
    ]
    IO.write("case expression: \n   x = [a]; case x do [] -> :ops; [hd|tl] -> hd end\n should result in {:ok, :a}\n")
    apply(module, :eval, [seq])
  end

  def test(module, 6) do
    seq = [
      {:match, {:var, :x}, {:atm, :a}},
      {:match, {:var, :f}, {:lambda, [:y], [:x], [{:cons, {:var, :x}, {:var, :y}}]}},
      {:apply, {:var, :f}, [{:atm, :b}]}
    ]
    IO.write("lambda expression: x = :a; f = fn(y) -> {x,y} end; f.(:b)  should result in {:ok, {:a, :b}}\n")
    apply(module, :eval, [seq])
  end

  ## Requires that append/2 and   are defined in Prgm module 
  
  def test(module, 7) do
    seq = [{:apply, {:fun, :append}, [{:atm, :nil}, {:atm, :nil}]}]
    IO.write("function call: \n    append(:nil, :nil) \n should result in {:ok, :nil}\n")
    apply(module, :eval, [seq])
  end

  def test(module, 8) do
    seq = [
      {:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, :nil}}}},
      {:match, {:var, :y}, {:cons, {:atm, :c}, {:cons, {:atm, :d}, {:atm, :nil}}}},
      {:apply, {:fun, :append}, [{:var, :x}, {:var, :y}]}
    ]
    IO.write("recursive function: \n   x = {:a, {:b, :nil}}; y = {:c, {:d, :nil}}; append(x, y) \nshould result in {:ok, {:a,{:b,{:c,{:d, :nil}}}}}\n")
    apply(module, :eval, [seq])
  end

  def test(module, 9) do
    seq = [
      {:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, :nil}}}},
      {:apply, {:fun, :nreverse}, [{:var, :x}]}
    ]
    IO.write("recursive function: \n x = {:a,{:b,:nil}}; nreverse(x)\n should result in {:ok, {:b, {:a,:nil}}}\n")
    apply(module, :eval, [seq])
  end

  def test(module, 10) do
    seq = [
      {:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, :nil}}}},
      {:match, {:var, :y}, {:atm, :nil}},
      {:match, {:var, :f}, {:lambda, [:p], [:y], [{:cons, {:var, :p}, {:var, :y}}]}},
      {:apply, {:fun, :map}, [ {:var, :x}, {:var, :f}]}
    ]
    IO.write("higher order\n   x = {:a, {:b,:nil}}; y = :nil; f = fn (p) -> {p, y} end; map(f,(x))\n should result in {:ok, {{:a,:nil}, {:b,:nil}}\n")
    apply(module, :eval, [seq])
  end

end
