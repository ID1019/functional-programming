defmodule DistMandel do

  def mandelbrot(width, height, x, y, k, depth) do
    trans = fn w, h ->
      Cmplx.new(x + k * (w - 1), y - k * (h - 1))
    end

    rows(width, height, trans, depth, self())
    collect(height, [])
  end

  defp collect(0, rows), do: rows
  defp collect(h, rows) do
    receive do
      {:row, ^h, row} ->
        collect(h - 1, [row | rows])
    end
  end

  defp rows(_, 0, _, _, _), do: :ok
  defp rows(w, h, tr, depth, ctrl) do
    spawn(fn -> report(w, h, tr, depth, ctrl) end)
    rows(w, h - 1, tr, depth, ctrl)
  end

  defp report(w, h, tr, depth, ctrl) do
    row = row(w, h, tr, depth, [])
    send(ctrl, {:row, h, row})
  end

  defp row(0, _, _, _, row), do: row
  defp row(w, h, tr, depth, row) do
    c = tr.(w, h)
    res = Brot.mandelbrot(c, depth)
    color = Color.convert(res, depth)
    row(w - 1, h, tr, depth, [color | row])
  end

end
