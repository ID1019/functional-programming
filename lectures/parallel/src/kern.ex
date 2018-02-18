defmodule Kern do

  def fold([], [], {r,g,b}) do
    {trim(r),trim(g),trim(b)}
  end
  def fold([k|kernel], [p|pixels], acc) do
    fold(kernel, pixels, add(k, p, acc))
  end

  def add(k, {r,g,b}, {ra,ga,ba}) do
    {k*r+ra, k*g+ga, k*b+ba}
  end

  def trim(r) do
    if r < 255 && r > 0 do
      trunc(r)
    else
      if r < 255 do
	0
      else
	255
      end
    end
  end
  

end
