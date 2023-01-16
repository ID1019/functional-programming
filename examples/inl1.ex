defmodule Inl do


  # do we cover the base case 

  def len(list) do
    [_|tail]=list
    case tail do
      []->1
      _->len(tail)+1
    end
  end


  # what should nth(3, [:a,:b]) return?
  
  def nth(n, l) do
    [head | tail] = l
    case n do
      1 -> head
      _ -> nth(n-1, tail)
    end
  end


  # how about this

  def nth2(_, []) do 0 end
  def nth2(_, [x]) do x end
  def nth2(0, [head | _tail]) do head end
  def nth2(n, l) do
    [_head | tail] = l
    nth2(n-1, tail)
  end


  # does this work

  def insert(elem, sorted) do
    [h | t] = sorted
    if elem < h do [elem | [h | t]] else [h | insert(elem, t)] end
  end

  #  yes look at this
  #
  #  iex(1)> Enkla.insert(5, [1,2,6,7]) [1, 2, 5, 6, 7]
  

  # rewrite using  <when>

  def insert(x,list) do
    case list do
      [] ->[x]
      _->([h|t] = list
      cond do
	  h>x -> [x|list]  
		 true->[h|insert(x,t)]
	end)
    end
  end


  # can we refactor the code

  def exp_mul(x, n) do
    cond do
      n == 0 -> 1
      n == 1 -> x
      rem(n, 2) == 0 ->
	exp_mul(x, div(n,2)) * exp_mul(x, div(n,2))
      rem(n, 2) == 1 ->
	exp_mul(x, n-1) * x
    end
  end  

		 
  
  # is this efficient? 

  def add(x, l) do
    case l do
      l -- [x] ->
	l ++ [x]
      _ ->
	l
    end
  end  

  # how do we express things? 

  def benchmark(function, x, n) do
    cond do
      function == :exp ->
	fn -> exp(x,n) end
	|> :timer.tc
	|> elem(0)
	|> Kernel./(1_000_000)
      function == :expFast ->
	fn -> expFast(x,n) end
	|> :timer.tc
	|> elem(0)
	|> Kernel./(1_000_000)
    end
  end








  

  
  def benchmark(fct, x, n) do
    case fct do
      :exp ->
	{t, _} = :timer.tc( fn -> exp(x,n) end )
	t/1_000_000
      :expFast ->
	{t, _} = :timer.tc( fn -> expFast(x,n) end )
	t/1_000_000
    end
  end
  


  
end
