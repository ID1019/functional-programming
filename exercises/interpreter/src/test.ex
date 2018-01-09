defmodule Test do

  def test(n) do
    ## The default is the eager module.
    test(Eager, n)
  end


  def test(module, 1) do
    seq = [{:atm, :a}]
    prgm = []
    :io.format("eval expression ~w~n   should result in {ok, a}~n", [seq])
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 2) do
    seq = [{:match, {:var, :x}, {:atm,:a}},
	   {:cons, {:var, :x}, {:atm,:b}}
	  ]
    prgm = []
    :io.format("eval expression ~w~n   should result in {:ok, [:a|:b]}~n", [seq])
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 3) do
    seq = [{:match, {:var,:x}, {:atm,:a}},
	   {:match, {:var, :y}, {:cons, {:var,:x}, {:atm,:b}}},
	   {:match, {:cons, :ignore, {:var, :z}}, {:var,:y}},
	   {:var, :z}
	  ]
    prgm = []
    :io.format("eval expression ~w~n   should result in {:ok, :b}~n", [seq])
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 4) do
    seq = [{:match, {:var, :x}, {:atm, :a}},
	   {:switch, {:var, :x},
	    [{:clause, {:atm, :b}, [{:atm, :ops}]},
	     {:clause, {:atm, :a}, [{:atm, :yes}]}
	    ]}
	  ]
    prgm = []
    :io.format("eval expression ~w~n   should result in {:ok, :yes}~n", [seq])
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 5) do
    seq = [{:match, {:var, :x}, {:cons, {:atm, :a}, {:atm, []}}},
	   {:switch, {:var, :x},
  	       [{:clause, {:atm, []}, [{:atm, :ops}]},
		{:clause, {:cons, {:var, :hd}, {:var, :tl}}, [{:var, :hd}]}
	    ]}
	  ]
    prgm = []
    :io.format("testing switch expression, should result in {ok, a}~n", [])
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 6) do
    seq = [{:call, :append, [{:atm, []}, {:atm, []}]}
          ]
    prgm = prg()
    :io.format("testing function application, should result in {ok, []}~n", [])
    apply(module, :eval, [seq, prgm])
  end
  
  def test(module, 7) do
    seq = [{:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, []}}}},
	   {:match, {:var, :y}, {:cons, {:atm, :c}, {:cons, {:atm, :d}, {:atm, []}}}},
	   {:call, :append, [{:var, :x}, {:var, :y}]}
	  ]
    prgm = prg()
    :io.format("testing recursive function, should result in {ok, [a,:b,:c,d]}~n", [])
    apply(module, :eval, [seq, prgm])
  end



  def test(module, 8) do
    seq = [{:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, []}}}},
	   {:call, :nreverse, [{:var, :x}]}
	  ]
    prgm =  prg()
    :io.format("nreverse of [a,b], should result in {ok, [:b,:a]}~n", [])
    apply(module, :eval, [seq, prgm])
  end

  def test(module, 9) do
    seq = [{:match, {:var, :x}, {:cons, {:atm, :a}, {:cons, {:atm, :b}, {:atm, []}}}},
	   {:match, {:var, :y}, {:atm, []}},
	   {:match, {:var, :f}, {:lambda, [:p], [:y], [{:cons, {:var, :p}, {:var,:y}}]}},
	   {:call, :map, [{:var, :f}, {:var, :x}]}
	  ]
    prgm =  prg()
    :io.format("higher order,  y = [], f = fn (p) -> [p|y] end, map(f,[:a,:b]), should result in {:ok, [[:a] [:b]]}~n", [])
    apply(module, :eval, [seq, prgm])
  end
  

  def prg() do [
    {:nreverse, [:x], 
     [{:switch, {:var, :x}, 
       [{:clause, {:atm, []}, [{:atm, []}]},
        {:clause, {:cons, {:var, :hd}, {:var, :tl}},
	 [{:call, :append, 
	   [{:call, :nreverse, [{:var, :tl}]}, 
	    {:cons, {:var, :hd}, {:atm, []}}]}
	 ]}
       ]}]},

    {:append, [:x, :y],
     [{:switch, {:var, :x}, 
       [{:clause, {:atm, []}, 
         [{:var, :y}]},
        {:clause, {:cons, {:var, :hd}, {:var, :tl}}, 
         [{:cons, {:var, :hd}, {:call, :append, [{:var, :tl}, {:var, :y}]}}]}]
      }]},

    {:map, [:f,:x], 
     [{:switch, {:var, :x}, 
       [{:clause, {:atm, []}, 
         [{:atm, []}]},
        {:clause, {:cons, {:var, :hd}, {:var, :tl}}, 
         [{:cons, {:apply, {:var, :f}, [{:var, :hd}]}, {:call, :map, [{:var, :f}, {:var, :tl}]}}]}]
      }]}
  ]
  end
  
  

  
end

