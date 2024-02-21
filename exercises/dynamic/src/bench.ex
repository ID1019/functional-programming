defmodule Bench do



  def bench(n) do

    Enum.reduce( 1..n , 0 , fn(i,p) ->
      {t,_} = :timer.tc(fn() -> Log.cust(Enum.to_list(1..i)) end)
      if p != 0 do
	:io.format(" n = ~w\t t = ~w\t us (~.2f)\n", [i, t, t/p])
	t
      else
	:io.format(" n = ~w\t t = ~w\t us\n", [i, t])
	t
      end
    end)
  end

      
  def log() do
    {t,_} = :timer.tc(fn() -> Log.cust([1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2]) end)
    IO.puts(" n = 20\t k = 2\t = #{t} us")
    {t,_} = :timer.tc(fn() -> Log.cust([1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5]) end)
    IO.puts(" n = 20\t k = 5\t = #{t} us")
    {t,_} = :timer.tc(fn() -> Log.cust([1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10]) end)
    IO.puts(" n = 20\t k = 10\t = #{t} us")
  end

      
  def wood() do
    {t,_} = :timer.tc(fn() -> Wood.cost([{10,1},{10,2}]) end)
    IO.puts(" n = 20\t k = 2\t = #{t} us")
    {t,_} = :timer.tc(fn() -> Wood.cost([{4,1},{4,2},{4,3},{4,4},{4,5}]) end)
    IO.puts(" n = 20\t k = 5\t = #{t} us")
    {t,_} = :timer.tc(fn() -> Wood.cost([{2,1},{2,2},{2,3},{2,4},{2,5},{2,6},{2,7},{2,8},{2,9},{2,10}]) end)
    IO.puts(" n = 20\t k = 10\t = #{t} us")
  end

  def wood(n, k) do
    :io.format("length of specs 1 to ~w \n", [k])
    Enum.reduce( 1..n, 0 , fn(i,p) ->
      {t,_} = :timer.tc(fn() -> Wood.cost(Enum.map(1..k, fn(k) -> {i, k} end)) end)
      if p != 0 do
	:io.format(" n = ~w\t t = ~w\t us (~.2f)\n", [i*k, t, t/p])
	t
      else
	:io.format(" n = ~w\t t = ~w\t us\n", [i*k, t])
	t
      end
    end)
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
