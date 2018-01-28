defmodule Cmplx do
  
  def new(x, y) do
    {:cpx, x, y}
  end

  def add({:cpx, x1, y1}, {:cpx, x2, y2}) do
    {:cpx, x1 + x2, y1 + y2}
  end

  def sqr({:cpx, x, y}) do
    {:cpx, x * x - y * y, 2 * x * y}
  end

  def abs({:cpx, x, y}) do
    :math.sqrt(x * x + y * y)
  end

  # Let's do the depth calculation as quickly as possible
  # in Elixir. We will avoid constructing tuples and do
  # as few operations as possible.
  def mandelbrot({:cpx, cr, ci}, m) do
    zr = 0
    zi = 0
    test(0, zr, zi, cr, ci, m)
  end

  defp test(m, _zr, _zi, _cr, _ci, m), do: 0
  defp test(i, zr, zi, cr, ci, m) do
    zr2 = zr * zr
    zi2 = zi * zi
    a2 = zr2 + zi2

    if a2 < 4.0 do
      sr = zr2 - zi2 + cr
      si = 2 * zr * zi + ci
      test(i + 1, sr, si, cr, ci, m)
    else
      i
    end
  end

  # This is doing the mandelbrot calculation using the NIF
  # interface. 
  def mandelbrot_nif({:cpx, cr, ci}, m) do
    Depth.test(cr, ci, m)
  end

end
