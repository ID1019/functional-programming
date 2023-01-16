defmodule Fib do

  def bench(l) do
    {:ok, file} = File.open("fib.dat", [:write, :list])
    seq = [12,14,16,18,20,22,24,26,28]
    :io.format(file, "# This is a comment since it starts with #\n", [])
    :io.format(file, "# Fibonacchi of n, time in us\n", [])
    :io.format(file, "# n\ttime\n", [])
    Enum.each(seq, fn n -> bench(l, n, file) end)
    File.close(file)
  end

  def bench(l, n, file) do
    {t, _} = :timer.tc(fn -> loop(l, n) end)
    :io.format(file, "~w\t~.2f\n", [n, t/l])
  end

  def loop(0,_) do :ok end
  def loop(l,n) do 
    fib(n)
    loop(l-1, n)
  end
  
  def fib(1) do 1 end
  def fib(2) do 1 end
  def fib(n) do fib(n-1) + fib(n-2) end

end
