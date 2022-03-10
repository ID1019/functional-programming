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

  ##   Julia set, these parameters can be choosen to generate different sets.

  @juliacr -0.8   
  @juliaci 0.156
  # cut off value, choosen so that R² - R > sqrt(cr² + ci²)
  #  R² >  (expt (+ 0.5  (sqrt (+ 0.25 (+ (expt 0.8 2) (expt 0.156 2)) ) ) ) 2)
  @juliaR2 2.13   

  #@juliacr -0.4   
  #@juliaci 0.6
  #  R² >  (expt (+ 0.5  (sqrt (+ 0.25 (+ (expt 0.4 2) (expt 0.6 2)) ) ) ) 2)
  #@juliaR2 1.9


  def julia({:cpx, cr, ci}, m) do
    julia(0, cr, ci, m)
  end

  defp julia(m, _zr, _zi, m), do: 0
  defp julia(i, zr, zi, m) do
    zr2 = zr * zr
    zi2 = zi * zi
    a2 = zr2 + zi2

    if a2 < @juliaR2 do 
      julia(i + 1, zr2 - zi2 + @juliacr, 2 * zr * zi  + @juliaci, m)
    else
      i
    end
  end
  

  
end
