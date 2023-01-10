defmodule Bench do



  def bench(n) do
    for i <- 1..n do
      {t,_} = :timer.tc(fn() -> Log.cust(Enum.to_list(1..i)) end)
      IO.puts(" n = #{i}\t t = #{t} us")
    end
  end

  def bench_k() do
    {t,_} = :timer.tc(fn() -> Log.cust([1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2]) end)
    IO.puts(" n = 20\t k = 2\t = #{t} us")
    {t,_} = :timer.tc(fn() -> Log.cust([1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5]) end)
    IO.puts(" n = 20\t k = 5\t = #{t} us")
    {t,_} = :timer.tc(fn() -> Log.cust([1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10]) end)
    IO.puts(" n = 20\t k = 10\t = #{t} us")
  end	


  def bench(n, k) do
    for i <- 1..n do
      {t,_} = :timer.tc(fn() -> Log.cust(Enum.map(1..i, fn(x) -> rem(x,k) + 1 end)) end)
      IO.puts(" n = #{i}\t t = #{t} us")
    end
  end


  def speedup(data) do
     {_, speed} = List.foldl(data, {1, []}, fn ({n,t},{p,a}) -> {t, [{n, t, t/p} | a]} end)
     Enum.each(Enum.reverse(speed), fn({n,t,r}) -> :io.format("~w\t &~.4f &~.1f \\\\ \n", [n, t/1000000, r]) end)
  
  end  


end
