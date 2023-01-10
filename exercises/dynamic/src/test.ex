defmodule Test do

  def split(seq) do split(seq, 0, [], []) end

  def split([], l, left, right)  do
    [{left, right, l}]
  end
  def split([s|rest], l, left, right) do
    split(rest, l+s, [s|left], right) ++
    split(rest, l+s, left, [s|right])
  end


end

