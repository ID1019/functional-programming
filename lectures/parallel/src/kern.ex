defmodule Kern do

  def fold([], [], {r,g,b}, depth) do
    {trim(r, depth),trim(g, depth),trim(b, depth)}
  end
  def fold([], [], ga, depth) do
    trim(ga, depth)
  end
  def fold([k|kernel], [p|pixels], acc, depth) do
    fold(kernel, pixels, add(k, p, acc), depth)
  end

  def add(k, {r,g,b}, {ra,ga,ba}) do
    {k*r+ra, k*g+ga, k*b+ba}
  end
  def add(k, g, ga) do
    k*g+ga
  end  

  def trim(r, depth) do
    if r < depth && r > 0 do
      trunc(r)
    else
      if r < depth do
	0
      else
	depth
      end
    end
  end
  

end
