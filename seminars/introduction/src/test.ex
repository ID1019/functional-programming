defmodule Test do

  def double(n) do
    2*n 
  end

  def product(m, n) do
    if  m == 0 do
      n
    else 
      product(m-1, n) + n
    end
  end


  def product_case(m, n) do
    case  m do
      0 -> 
	n
      _ -> 
	product_case(m-1, n) + n 
    end
  end

  def product_clauses(0, _) do 0 end
  def product_clauses(m, n) do 
    product_clauses(m-1,n) + n
  end

  def exp(_,0) do 1 end
  def exp(x,n) do
    x*exp(x, n-1)
  end

  def faster(_,0) do 1 end
  def faster(x,n) do
    case rem(n,2) do
      0 ->
	e = faster(x, div(n,2))
	e*e
      1 ->
	faster(x,n-1)*x
    end
  end
  
  def append(x, y) do
    case x do
      [] -> y
      [h|t] -> [h | append(t, y)]
    end
  end
 
  def nreverse([]) do [] end
  def nreverse([h|t]) do 
    r = nreverse(t)
    append(r, [h])
  end


  def reverse(l) do
   reverse(l, [])
  end

  def reverse([], r) do  r end
  def reverse([h|t], r) do  reverse(t, [h|r]) end


 
  def bench() do
    ls = [16, 32, 64, 128, 256, 512]
    n = 100
    # bench is a closure - function with an environment
    bench = fn(l) ->
      seq = :lists.seq(1,l)
      tn = time(n, fn() -> nreverse(seq) end)
      tr = time(n, fn() -> reverse(seq) end)
      :io.format("length: ~10w  nrev: ~8w us    rev: ~8w us~n", [l, tn, tr])
    end
    # We use the library function Enum.each that 
    # will call bench(l) for each element l in ls
    Enum.each(ls, bench)
  end

  def time(n, fun) do
    start = System.monotonic_time(:milliseconds)
    loop(n, fun)
    stop = System.monotonic_time(:milliseconds)
    (stop - start)
  end

  def loop(n, fun) do
    if n == 0 do
      :ok 
    else
      fun.()
      loop(n-1, fun) 
    end
  end

  
  def to_binary(0) do [] end
  def to_binary(n) do
     append(to_binary(div(n,2)), [rem(n,2)])
  end

  def to_better(n) do
    to_better(n, [])
  end

  def to_better(0, b) do b end
  def to_better(n, b) do 
    to_better(div(n,2), [rem(n,2)|b])
  end  

  
  def to_integer(x) do
    to_integer(x,0)
  end
  
  def to_integer([], n) do n end
  def to_integer([x|r], n) do
    to_integer(r, 2*n+x)
  end
  


end


