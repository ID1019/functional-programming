defmodule Bench do

  def dummy(_,_) do
    :ok
  end

  def nth_r(1, [h|_]) do h end
  def nth_r(n, [_|t]) do  nth_r(n-1, t) end

  def nth_e(n, t) do
    elem(t, n-1)
  end


  def nth_l( 1, [n|_]) do n end
  def nth_l( 2, [_,n|_]) do n end
  def nth_l( 3, [_,_,n|_]) do n end
  def nth_l( 4, [_,_,_,n|_]) do n end
  def nth_l( 5, [_,_,_,_,n|_]) do n end
  def nth_l( 6, [_,_,_,_,_,n|_]) do n end
  def nth_l( 7, [_,_,_,_,_,_,n|_]) do n end
  def nth_l( 8, [_,_,_,_,_,_,_,n|_]) do n end
  def nth_l( 9, [_,_,_,_,_,_,_,_,n|_]) do n end
  def nth_l(10, [_,_,_,_,_,_,_,_,_,n]) do n end

  def nth_t( 1, {n,_,_,_,_,_,_,_,_,_}) do n end
  def nth_t( 2, {_,n,_,_,_,_,_,_,_,_}) do n end
  def nth_t( 3, {_,_,n,_,_,_,_,_,_,_}) do n end
  def nth_t( 4, {_,_,_,n,_,_,_,_,_,_}) do n end
  def nth_t( 5, {_,_,_,_,n,_,_,_,_,_}) do n end
  def nth_t( 6, {_,_,_,_,_,n,_,_,_,_}) do n end
  def nth_t( 7, {_,_,_,_,_,_,n,_,_,_}) do n end
  def nth_t( 8, {_,_,_,_,_,_,_,n,_,_}) do n end
  def nth_t( 9, {_,_,_,_,_,_,_,_,n,_}) do n end
  def nth_t(10, {_,_,_,_,_,_,_,_,_,n}) do n end

  def bench() do
    n = 1000000
    ls = 1..10
    :io.format("# Benchmark for finding n'th element, ~w times, in a list or tuple, all times in ms.~n", [n])
    :io.format("#~8s ~8s ~8s ~8s ~8s ~8s~n", ["n", "dummy", "recursive", "explicit", "elem/2", "tuple"])
    bench = fn (l) ->
      list = Enum.to_list(1..10)
      tuple = List.to_tuple(list)
      td = time(n, fn() -> dummy(l,list) end)
      tr = time(n, fn() -> nth_r(l,list) end)
      tl = time(n, fn() -> nth_l(l,list) end)
      te = time(n, fn() -> nth_e(l,tuple) end)
      tt = time(n, fn() -> nth_t(l,tuple) end) 
      :io.format(" ~8w ~8.2f ~8.2f ~8.2f ~8.2f ~8.2f~n", [l, td/1000, tr/1000, tl/1000, te/1000, tt/1000])
    end
    time(n, fn() -> dummy(nil, nil) end)
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

