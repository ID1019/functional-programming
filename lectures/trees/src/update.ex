defmodule Update do

  def dummy(_,_,_) do
    :ok
  end

  def update([_|t], 1, k ) do [k|t] end
  def update([h|t], n, k ) do [h|update(t, n-1, k)] end


  def bench(k) do
    n = 1_000_000
    ls = [8,16,32,64,128,256,512,1024]

    :io.format("# Benchmark for updating an element, ~w times, in a list or tuple, all times in ms.~n", [n])
    :io.format("# The element is at position n/~w\n", [k])
    :io.format("#~8s ~8s ~8s ~8s~n", ["n", "dummy", "list", "tuple"])

    bench = fn (l) ->

      list = Enum.to_list(1..l)
      tuple = List.to_tuple(list)

      i = div(l, k)
      i = if (k > l) do
	1
      else
	div(l,k)
      end
      
      
      td = time(n, fn() -> dummy(nil, nil, nil) end)
      tl = time(n, fn() -> update(list,i,0) end)
      tt = time(n, fn() -> put_elem(tuple, i-1,0) end)

      :io.format(" ~8w ~8.2f ~8.2f ~8.2f~n", [l, td/1000, tl/1000, tt/1000])
    end

    Enum.each(ls, bench)
  end

  def time(n, f) do
    elem(:timer.tc(fn () -> loop(n, f) end), 0)
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

