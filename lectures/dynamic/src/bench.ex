defmodule Bench do

  def memory() do
    ## this enough, searching will take 30 s
    memory(7)
  end

  def map() do
    ## this enough, searching will take 30 s
    map(7)
  end  


  
  def memory(n)  do
    all = Enum.map(1..n, fn (x) -> {x * 1000, x * 200} end)
    {:ok, fd} = :file.open("memory.dat", [:write])
    :io.format(fd, "%~9s ~10s ~10s ~10s ~10s~n", ["m", "t", "m x t", "search", "memory"])
    Enum.each( all,
      fn ({m,t}) ->
	:erlang.garbage_collect()
	{t1, _} = :timer.tc(fn() -> Hinges.search(m, t, {260, 40, 30}, {180, 60, 24}) end)
	:erlang.garbage_collect()
      {t2, _} = :timer.tc(fn() -> Hinges.memory(m, t, {260, 40, 30}, {180, 60, 24}) end)
      :io.format(fd, "~10w ~10w ~10w ~10.2f ~10.2f~n", [m, t, (t+m), t1/1000, t2/1000])
      end)
    :file.close(fd)
  end

  def map(n)  do
    all = Enum.map(5..n, fn (x) -> {x * 1000, x * 200} end)
    {:ok, fd} = :file.open("map.dat", [:write])
    :io.format(fd, "%~9s ~10s ~10s ~10s ~10s~n", ["m", "t", "m x t", "linear", "map"])
    Enum.each( all,
      fn ({m,t}) ->
	:erlang.garbage_collect()
	{t1, _} = :timer.tc(fn() -> Enum.each(1..1000, fn(_) -> Hinges.memory(m, t, {260, 40, 30}, {180, 60, 24}) end) end)
	:erlang.garbage_collect()
      {t2, _} = :timer.tc(fn() -> Enum.each(1..1000, fn(_) -> Hinges.map(m, t, {260, 40, 30}, {180, 60, 24}) end) end)
      :io.format(fd, "~10w ~10w ~10w ~10.2f ~10.2f~n", [m, t, (t+m), t1/1000, t2/1000])
      end)
    :file.close(fd)
  end



end
