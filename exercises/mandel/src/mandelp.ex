defmodule Mandelp do


  def mandelbrot(width, height, x, y, k, depth) do
    trans = fn(w,h) -> {x + k*(w-1), y-k*(h-1)} end
    rows(width, height, trans, depth, self())
    collect(height, [])
  end


  def collect(0, rows) do
    rows
  end
  def collect(h, rows) do
    receive do
      {:row, ^h, row} ->
	collect(h-1, [row|rows])
    end
  end


  def rows( _, 0, _, _, _) do
    :ok
  end
  def rows(w, h, tr, depth, ctrl) do
    spawn(fn() -> report(w, h, tr, depth, ctrl) end)
    rows(w, h-1, tr, depth, ctrl)
  end


  def report(w, h, tr, depth, ctrl) do
    row = row(w, h, tr, depth, [])
    send(ctrl,{:row, h, row})
  end

  def row(0, _, _, _, row) do
    row
  end
  def row(w, h, tr, depth, row) do
    {x,y} = tr.(w,h)
    xy = Cmplx.new(x,y)
    res =  Brot.mandelbrot(xy, depth)
    color = Color.convert(res, depth)
    row(w-1, h, tr, depth, [color|row])
  end
end


    



