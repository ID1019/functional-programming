defmodule Test do

  # Append the hard way.
  def append(a, b) do
    app = fn x, y, f ->
      case x do
        [] -> y
        [h | t] -> [h | f.(t, y, f)]
      end
    end

    app.(a, b, app)
  end

  # Fibonacci sequence.
  def fib(n) do
    fib = fn x, f ->
      case x do
        0 -> 0

        1 -> 1

        d -> f.(d - 1, f) + f.(d - 2, f)
      end
    end

    fib.(n, fib)
  end
  
end
