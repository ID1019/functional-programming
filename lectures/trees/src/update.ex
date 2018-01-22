defmodule Update do

  def dummy(_,_,_) do
    :ok
  end

  def nth_r(1, k, [_|t]) do [k|t] end
  def nth_r(n, k, [h|t]) do [h|nth_r(n-1, k , t)] end

  def nth_e(n, k, t) do
    i = n-1
    Tuple.insert_at(Tuple.delete_at( t, i), i, k)
  end


  def nth_l( 1, k, [_|t]) do
    [k|t]
  end
  def nth_l( 2, k, [a,_|t]) do
    [a,k|t]
  end
  def nth_l( 3, k, [a,b,_|t]) do
    [a,b,k|t]
  end
  def nth_l( 4, k, [a,b,c,_|t]) do
    [a,b,c,k|t]
  end
  def nth_l( 5, k, [a,b,c,d,_|t]) do
    [a,b,c,d,k|t]
  end
  def nth_l( 6, k, [a,b,c,d,e,_|t]) do
    [a,b,c,d,e,k|t]
  end
  def nth_l( 7, k, [a,b,c,d,e,f,_|t]) do
    [a,b,c,d,e,f,k|t]
  end
  def nth_l( 8, k, [a,b,c,d,e,f,g,_|t]) do
    [a,b,c,d,e,f,g,k|t]
  end
  def nth_l( 9, k, [a,b,c,d,e,f,g,h,_|t]) do
    [a,b,c,d,e,f,g,h,k|t]
  end
  def nth_l(10, k, [a,b,c,d,e,f,g,h,i,_]) do
    [a,b,c,d,e,f,g,h,i,k]
  end


  def nth_t( 1, k, {_,b,c,d,e,f,g,h,i,j}) do
    {k,b,c,d,e,f,g,h,i,j}
  end
  def nth_t( 2, k, {a,_,c,d,e,f,g,h,i,j}) do
    {a,k,c,d,e,f,g,h,i,j}
  end
  def nth_t( 3, k, {a,b,_,d,e,f,g,h,i,j}) do
    {a,b,k,d,e,f,g,h,i,j}
  end
  def nth_t( 4, k, {a,b,c,_,e,f,g,h,i,j}) do
    {a,b,c,k,e,f,g,h,i,j}
  end
  def nth_t( 5, k, {a,b,c,d,_,f,g,h,i,j}) do
    {a,b,c,d,k,f,g,h,i,j}
  end
  def nth_t( 6, k, {a,b,c,d,e,_,g,h,i,j}) do
    {a,b,c,d,e,k,g,h,i,j}
  end
  def nth_t( 7, k, {a,b,c,d,e,f,_,h,i,j}) do
    {a,b,c,d,e,f,k,h,i,j}
  end
  def nth_t( 8, k, {a,b,c,d,e,f,g,_,i,j}) do
    {a,b,c,d,e,f,g,k,i,j}
  end
  def nth_t( 9, k, {a,b,c,d,e,f,g,h,_,j}) do
    {a,b,c,d,e,f,g,h,k,j}
  end
  def nth_t(10, k, {a,b,c,d,e,f,g,h,i,_}) do
    {a,b,c,d,e,f,g,h,i,k}
  end    


  def bench() do
    n = 1000000
    ls = 1..10
    :io.format("# Benchmark for updating the n'th element, ~w times, in a list or tuple, all times in ms.~n", [n])
    :io.format("#~8s ~8s ~8s ~8s ~8s ~8s~n", ["n", "dummy", "recursive", "explicit", "elem/2", "tuple"])
    bench = fn (l) ->
      list = Enum.to_list(1..10)
      tuple = List.to_tuple(list)
      td = time(n, fn() -> dummy(l,0,list) end)
      tr = time(n, fn() -> nth_r(l,0,list) end)
      tl = time(n, fn() -> nth_l(l,0,list) end)
      te = time(n, fn() -> nth_e(l,0,tuple) end)
      tt = time(n, fn() -> nth_t(l,0,tuple) end) 
      :io.format(" ~8w ~8.2f ~8.2f ~8.2f ~8.2f ~8.2f~n", [l, td/1000, tr/1000, tl/1000, te/1000, tt/1000])
    end
    time(n, fn() -> dummy(nil, nil, nil) end)
    Enum.each(ls, bench)
  end

  def time(n, f) do
    elem(:timer.tc(fn () -> loop(n, f) end),0)
  end

  def loop(n, bench) do
    if n == 0 do
      :ok
    else
      bench.()
      loop(n-1, bench)
    end
  end

end

