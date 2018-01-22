defmodule Large do

  def dummy(_,_,_) do
    :ok
  end

  def nth_r(1, k, [_|t]) do [k|t] end
  def nth_r(n, k, [h|t]) do [h|nth_r(n-1, k , t)] end

  def nth_e(n, k, t) do
    i = n-1
    Tuple.insert_at(Tuple.delete_at( t, i), i, k)
  end

  def bench() do
    n = 1000000
    ls = [10,20,40,80,160,320,640,1280]
    i = 1
    :io.format("# Benchmark for updating the ~w element in a n'th element structure, ~w times, in a list or tuple, all times in ms.~n", [i, n])
    :io.format("#~8s ~8s ~8s ~8s~n", ["n", "dummy", "list", "tuple"])
    bench = fn (l) ->
      list = Enum.to_list(1..l)
      tuple = List.to_tuple(list)
      td = time(n, fn() -> dummy(i,0,list) end)
      tr = time(n, fn() -> nth_r(i,0,list) end)
      te = time(n, fn() -> nth_e(i,0,tuple) end)
      :io.format(" ~8w ~8.2f ~8.2f ~8.2f~n", [l, td/1000, tr/1000, te/1000])
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

