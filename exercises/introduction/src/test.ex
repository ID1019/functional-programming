defmodule Test do

  # Compute the double of a number.
  def double(n) do
    2 * n
  end

  # Compute the product of m and n recursively.
  # Option 1: if-else
  def product(m, n) do
    if m == 0 do
      0
    else
      product(m - 1, n) + n
    end
  end

  # Compute the product of m and n recursively.
  # Option 2: case
  def product_case(m, n) do
    case m do
      0 ->
        0
      _ ->
        product_case(m - 1, n) + n
    end
  end

  # Compute the product of m and n recursively.
  # Option 3: cond-do
  def product_cond(m, n) do
    cond do
      m == 0 ->
        0
      true ->
        product_cond(m - 1, n) + n
    end
  end

  # Compute the product of m and n recursively.
  # Option 4: multiple clauses
  def product_clauses(0, _) do 0 end
  def product_clauses(m, n) do
    product_clauses(m - 1, n) + n
  end

  # Compute the nth power of x.
  # Option 1: multiple clauses
  def exp(_, 0) do 1 end
  def exp(x, n) do
    x * exp(x, n - 1)
  end

  # Compute the nth power of x.
  # Option 2: case
  def exp_faster(_, 0) do 1 end
  def exp_faster(x, n) do
    case rem(n, 2) do
      0 ->
        e = exp_faster(x, div(n, 2))
        e * e

      1 ->
        exp_faster(x, n - 1) * x
    end
  end

  # Append an element at the end of a list.
  # Usage: append(list, element)
  def append(x, y) do
    case x do
      [] -> 
        y
      [h | t] ->
        [h | append(t, y)]
    end
  end

  # Reverse a list.
  # Option 1: simple recursive approach
  def nreverse([]) do [] end
  def nreverse([h | t]) do
    r = nreverse(t)
    append(r, [h])
  end

  # Reverse a list.
  # Option 2: tail-recursive approach
  def reverse(l) do
    reverse(l, [])
  end
  def reverse([], r) do r end
  def reverse([h | t], r) do
    reverse(t, [h | r])
  end

  # Benchmark for the list reverse functions.
  def bench() do
    ls = [16, 32, 64, 128, 256, 512]
    n = 100
    # bench is a closure: a function with an environment.
    bench = fn(l) ->
      seq = Enum.to_list(1..l)
      tn = time(n, fn -> nreverse(seq) end)
      tr = time(n, fn -> reverse(seq) end)
      :io.format("length: ~10w  nrev: ~8w us    rev: ~8w us~n", [l, tn, tr])
    end

    # We use the library function Enum.each that will call
    # bench(l) for each element l in ls
    Enum.each(ls, bench)
  end

  # Time the execution time of the a function.
  def time(n, fun) do
    start = System.monotonic_time(:milliseconds)
    loop(n, fun)
    stop = System.monotonic_time(:milliseconds)
    stop - start
  end

  # Apply the function n times.
  def loop(n, fun) do
    if n == 0 do
      :ok
    else
      fun.()
      loop(n - 1, fun)
    end
  end

  # Convert an integer to a binary.
  def to_binary(0) do [] end
  def to_binary(n) do
    append(to_binary(div(n, 2)), [rem(n, 2)])
  end

  # Convert integer to binary with a more performant
  # solution.
  def to_better(n) do to_better(n, []) end
  def to_better(0, b) do b end
  def to_better(n, b) do
    to_better(div(n, 2), [rem(n, 2) | b])
  end

  # Convert a binary to an integer.
  def to_integer(x) do to_integer(x, 0) end
  def to_integer([], n) do n end
  def to_integer([x | r], n) do
    to_integer(r, 2 * n + x)
  end

  # Compute Fibonacci sequence.
  def fib(0) do 0 end
  def fib(1) do 1 end
  def fib(n) do fib(n - 1) + fib(n - 2) end

  # Benchmark for Fibonacci sequence.
  def bench_fib() do
    ls = [8,10,12,14,16,18,20,22,24,26,28,30,32]
    n = 10

    bench = fn(l) ->
      t = time(n, fn() -> fib(l) end)
      :io.format("n: ~4w  fib(n) calculated in: ~8w us~n", [l, t])
    end

    Enum.each(ls, bench)
  end
end
