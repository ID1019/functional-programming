defmodule Test do

  def test(n) do
    # The default is the eager module.
    test(Eager, n)
  end

  def test(module, 1) do
    seq = [{:atm, :a}]
    prgm = []
    IO.write("atom: \n    :a  \nshould result in {:ok, :a}\n")
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 2) do
    seq = [{:match, {:var, :x}, {:atm, :a}}, {:cons, {:var, :x}, {:atm, :b}}]
    prgm = []
    IO.write("sequence:\n  x = :a; [x|:b]\nshould result in {:ok, [:a | :b]}\n")
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 3) do
    seq = [
      {:match, {:var, :x}, {:atm, :a}},
      {:match, {:var, :y}, {:cons, {:var, :x}, {:atm, :b}}},
      {:match, {:cons, :ignore, {:var, :z}}, {:var, :y}},
      {:var, :z}
    ]
    prgm = []
    IO.write("sequence:\n x = :a; y = [ x | :b ];  [_ | z] = y; z\n   should result in {:ok, :b}\n")
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 4) do
    seq = [
      {:match, {:var, :x}, {:atm, :a}},
      {:case, {:var, :x},
       [{:clause, {:atm, :b}, [{:atm, :ops}]}, {:clause, {:atm, :a}, [{:atm, :yes}]}]}
    ]
    prgm = []
    IO.write("case expression: \n x = :a; case x do :b -> :ops; :a -> :yes end \nshould result in {:ok, :yes}\n")
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 5) do
    seq = [
      {:match, {:var, :x}, {:cons, {:atm, :a}, {:atm, []}}},
      {:case, {:var, :x},
       [
         {:clause, {:atm, []}, [{:atm, :ops}]},
         {:clause, {:cons, {:var, :hd}, {:var, :tl}}, [{:var, :hd}]}
       ]}
    ]
    prgm = []
    IO.write("case expression: \n   x = [a]; case x do [] -> :ops; [hd|tl] -> hd end\n should result in {:ok, :a}\n")
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 6) do
    seq = [
      {:match, {:var, :x}, {:atm, :a}},
      {:match, {:var, :f}, {:lambda, [:y], [:x], [{:cons, {:var, :x}, {:var, :y}}]}},
      {:apply, {:var, :f}, [{:atm, :b}]}
    ]
    prgm = []
    IO.write("lambda expression: x = :a; f = fn(y) -> [x|y] end; f.(:b)  should result in {:ok, [:a | :b]}\n")
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 7) do
    seq = [{:call, :append, [{:atm, []}, {:atm, []}]}]
    prgm = prg()
    IO.write("function call: \n    append([], []) \n should result in {:ok, []}\n")
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 8) do
    seq = [
      {:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, []}}}},
      {:match, {:var, :y}, {:cons, {:atm, :c}, {:cons, {:atm, :d}, {:atm, []}}}},
      {:call, :append, [{:var, :x}, {:var, :y}]}
    ]
    prgm = prg()
    IO.write("recursive function: \n   x = [:a, :b]; y = [:c, :d]; append(x, y) \nshould result in {:ok, [:a, :b, :c, :d]}\n")
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 9) do
    seq = [
      {:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, []}}}},
      {:call, :nreverse, [{:var, :x}]}
    ]
    prgm = prg()
    IO.write("recursive function: \n x = [:a,:b]; nreverse(x)\n should result in {:ok, [:b, :a]}\n")
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 10) do
    seq = [
      {:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, []}}}},
      {:match, {:var, :y}, {:atm, []}},
      {:match, {:var, :f}, {:lambda, [:p], [:y], [{:cons, {:var, :p}, {:var, :y}}]}},
      {:call, :map, [{:var, :f}, {:var, :x}]}
    ]
    prgm = prg()
    IO.write("higher order\n   x = [:a, :b]; y = []; f = fn (p) -> [p | y] end; map(f,(x))\n should result in {:ok, [[:a] [:b]]}\n")
    apply(module, :eval, [seq, prgm])
  end

  def prg() do
    [
      {:nreverse, [:x],
       [
         {:case, {:var, :x},
          [
            {:clause, {:atm, []}, [{:atm, []}]},
            {:clause, {:cons, {:var, :hd}, {:var, :tl}},
             [
               {:call, :append,
                [{:call, :nreverse, [{:var, :tl}]}, {:cons, {:var, :hd}, {:atm, []}}]}
             ]}
          ]}
       ]},
      {:append, [:x, :y],
       [
         {:case, {:var, :x},
          [
            {:clause, {:atm, []}, [{:var, :y}]},
            {:clause, {:cons, {:var, :hd}, {:var, :tl}},
             [{:cons, {:var, :hd}, {:call, :append, [{:var, :tl}, {:var, :y}]}}]}
          ]}
       ]},
      {:map, [:f, :x],
       [
         {:case, {:var, :x},
          [
            {:clause, {:atm, []}, [{:atm, []}]},
            {:clause, {:cons, {:var, :hd}, {:var, :tl}},
             [
               {:cons, {:apply, {:var, :f}, [{:var, :hd}]},
                {:call, :map, [{:var, :f}, {:var, :tl}]}}
             ]}
          ]}
       ]}
    ]
  end

end
