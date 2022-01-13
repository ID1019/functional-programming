defmodule Primes do

  defstruct [:next]
    
  defimpl Enumerable do

    def count(_) do  {:error, __MODULE__}  end
    def member?(_, _) do {:error, __MODULE__}  end
    def slice(_) do {:error, __MODULE__} end

    def reduce(_,       {:halt, acc}, _fun),   do: {:halted, acc}
    def reduce(primes,  {:suspend, acc}, fun), do: {:suspended, acc, fn(cmd) -> reduce(primes, cmd, fun) end}
    def reduce(primes,  {:cont, acc}, fun) do
      {p, next} = Primes.next(primes)
      reduce(next, fun.(p,acc), fun)
    end      

  end

  def next(%Primes{next: n}) do 
    {p, n} = n.()
    {p, %Primes{next: n}}
  end
  
  def primes() do
    %Primes{next: fn () ->  {2, fn () -> sieve(z(3), 2)  end} end}
  end

  def z(n) do
    fn() ->
      :io.format("generate ~w\n", [n])
       {n, z(n+1)}
     end
  end

  def sieve(n, p) do
    {c, f} = filter(n, p)
    {c, fn() -> filter(f, c) end}
  end

  def filter(n, p) do
    {c, n} = n.()
    :io.format(" check if ~w is divisable by ~w \n", [c, p])
    if rem(c,p) == 0 do
      :io.format(" discard ~w\n", [c])
      filter(n, p)
    else
      :io.format(" accept ~w\n", [c])
      {c, fn() -> filter(n,p) end}
    end
  end
  

end

