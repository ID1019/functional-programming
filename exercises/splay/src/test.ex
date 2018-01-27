defmodule Test do

  def test() do
    insert = [7, 2, 5, 3, 9, 4, 8, 1, 0, 4, 3, 5, 4, 6, 5, 4, 8, 2, 3, 7, 8, 5, 4, 3, 6]
    tree = Splay.tree()
    updated = List.foldl(insert, tree, fn(k, a) -> Splay.update(a, k, k) end)

    List.foldl(insert, updated, fn(k, a) ->
      {:ok, ^k, u} = Splay.lookup(a, k)
      u
    end)

    {:ok, 9} = ordered(updated, -1)
    traverse(updated)
  end

  def test(insert) do
    tree = Splay.tree()
    updated = List.foldl(insert, tree, fn(k, a) -> Splay.update(a, k, k) end)

    List.foldl(insert, updated, fn(k, a) ->
      {:ok, ^k, u} = Splay.lookup(a, k)
      u
    end)

    {:ok, 9} = ordered(updated, -1)
    traverse(updated)
  end

  defp ordered(nil, s), do: {:ok, s}
  defp ordered({:node, k, _, l, r}, s) do
    case ordered(l, s) do
      {:ok, s1} ->
        cond do
          s1 < k ->
            ordered(r, k)

          true ->
            false
        end

      false ->
        false
    end
  end

  defp traverse(t), do: traverse(t, [])
  defp traverse(nil, s), do: s
  defp traverse({:node, k, _, l, r}, s) do
    traverse(l, [k | traverse(r, s)])
  end

end
