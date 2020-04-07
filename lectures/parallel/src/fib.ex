defmodule Fib do

  def sequential(n) do
    {t,f} = :timer.tc(fn() -> seq(n) end)
    IO.puts("sequential fib(#{n}) = #{f}  in #{trunc(t/1000)} ms")
    :ok
  end

  def parallel(n) do
    {t,f} = :timer.tc(fn() -> par(n) end)
    IO.puts("parallel fib(#{n}) = #{f}  in #{trunc(t/1000)} ms")
    :ok
  end

  def combined(n, k) do
    {t,f} = :timer.tc(fn() -> comb(n, k) end)
    IO.puts("combined fib(#{n}) = #{f}  in #{trunc(t/1000)} ms")
    :ok
  end  

  def comb(0,_) do 0 end
  def comb(1,_) do 1 end
  def comb(n, k) when n > k do
    r1 = paral(fn() -> comb(n-1, k) end)
    r2 = paral(fn() -> comb(n-2, k) end)    
    f1 = collect(r1)
    f2 = collect(r2)
    f1 + f2
  end
  def comb(n, _) do
    seq(n)
  end


  
  def seq(0) do 0 end
  def seq(1) do 1 end
  def seq(n) do
    seq(n-1) + seq(n-2)
  end

  def par(0) do 0 end
  def par(1) do 1 end
  def par(n) do
    r1 = paral(fn() -> par(n-1) end)
    r2 = paral(fn() -> par(n-2) end)    
    f1 = collect(r1)
    f2 = collect(r2)
    f1 + f2
  end    

  def paral(f) do
    ref = make_ref()
    me = self()
    spawn(fn() -> send(me, {:par, ref, f.()}) end)
    ref
  end
      
  def collect(ref) do
    receive do
      {:par, ^ref, res} -> res
    end
  end


  def bench(n, k) do
    Enum.each(n..k, fn(s) ->
      {c,_} = :timer.tc(fn() -> comb(n, s) end)
      IO.puts(" k = #{s} in #{trunc(c/1000)} ms")
    end)
    :ok
  end


  def graph(p) do
    {:ok, fd} = File.open("fib.dat", [:write])
    IO.write(fd, "# fib(40,30), exeution time in ms\n")
    IO.write(fd, "#proc\tmin\tq2\tq4\tmax\n")    
    Enum.each(1..p, fn(p) -> graph(p, fd) end)
    File.close(fd)
  end

  def graph(p, fd) do
    :erlang.system_flag(:schedulers_online, p)
    res = Enum.map(1..100, fn(_) -> {t,_} = :timer.tc(fn() -> comb(40,30) end); t end)
    res = Enum.sort(res)
    min = trunc(:lists.nth(1, res)/1000)
    q2 = trunc(:lists.nth(25, res)/1000)
    q4 = trunc(:lists.nth(75, res)/1000)
    max = trunc(:lists.nth(100,res)/1000)
    IO.write(fd, "#{p}\t#{min}\t#{q2}\t#{q4}\t#{max}\n")
  end

  
end
