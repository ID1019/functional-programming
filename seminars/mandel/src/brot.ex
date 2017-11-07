defmodule Brot do


  ## This is the client version

  def start(Mandel) do
    {:ok, spawn fn -> init(Mandel) end}
  end

  defp init(Mandel) do
    Self = self()
    brot(Mandel, Self)
  end
    

  defp brot(Mandel, Self) do
    send(Mandel, {:req, Self})
    receive do
      {:pos, Pr, Pos, C, M} ->
	I = mandelbrot(C, M)
	send(Pr, {:res, Pos, I})
	brot(Mandel, Self)
      :done ->
	:ok
    end
  end

### mandelbrot(c,m) : calculate the mandelbrot value of complex value c
### with a maximum iteration of m. Returns 0..(m-1)



  ## This is the vanilla version

  def mandelbrot(c, m) do
       z0 = Cmplx.new(0,0)
      test(0, z0, c, m)
  end
  
  def test(m, _z, _c, m) do 0 end
  def test(i, z, c, m) do 
    a = Cmplx.abs(z)
    if a < 2.0 do
      z1 = Cmplx.add(Cmplx.sqr(z), c)
      test(i+1, z1, c, m)
    else 
       i
    end
  end

  ## This is using the Cmplx version of the calculation.
  
  def mandelbrot_cmplx(c, m) do
    Cmplx.mandelbrot(c,m)
  end

  ## This is if we want to use the native code version

  def mandelbrot_nif(c, m) do
    Cmplx.mandelbrot_nif(c, m)
  end
  
end

