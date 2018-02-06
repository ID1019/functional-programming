defmodule Bench do


  def bench()  do
    n = 7  ## this is more than enough
    all = Enum.map(1..n, fn (x) -> {x * 1000, x * 200} end)
    {:ok, fd} = :file.open("memory.dat", [:write])
    :io.format(fd, "%~9s ~10s ~10s ~10s ~10s~n", ["m", "t", "N", "search", "memory"])
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



end
