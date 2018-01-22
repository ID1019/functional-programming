defmodule Queue do

  def test() do
    ls = Enum.to_list(1..20)
    empty = {:queue, [], []}
    updated = List.foldl(ls, empty, fn(i,a) ->  add(a, i) end)
    List.foldl( ls, {:ok, nil, updated}, fn(_,{:ok, _, a}) ->  remove(a) end)
  end
  
  def add({:queue, front, back}, elem) do
    {:queue, front, [elem|back]}
  end

  def remove({:queue, [], []}) do :fail end

  def remove({:queue, [elem|rest], back}) do
    {:ok, elem, {:queue, rest, back}}
  end

  def remove({:queue, [], back}) do
    [elem|rest] = reverse(back)
    {:ok, elem, {:queue, rest, []}}
  end

  defp reverse(l) do reverse(l, []) end

  defp reverse([], r) do r end
  defp reverse([h|t], r) do
    reverse(t, [h|r])
  end
  
end
