defmodule Test do

  def bench(n) do
    seq = sequence(n, 100 * n)
    {ad, am} = bench(seq, Avl)
    IO.puts("AVL tree of max depth #{am} and average depth #{ad/n}")
    {bd, bm} = bench(seq, Bst)
    IO.puts("BST tree of max depth #{bm} and average depth #{bd/n}")
  end
  defp bench(seq, module) do
    empty = apply(module, :tree, [])
    inserted = List.foldl(seq, empty, fn(k, a) -> apply(module, :insert, [a, k, k]) end)
    total = List.foldl(seq, 0, 
      fn(k, a) -> 
        {:ok, d} = apply(module, :depth, [inserted, k])
        d + a 
      end)
    max = apply(module, :max_depth, [inserted])
    {total, max}
  end

  def time(n) do
    seq = sequence(n, 100 * n)
    {a1, a2, ad, am} = time(seq, Avl)
    IO.puts("AVL tree of max depth #{am} and average depth #{ad/n} - constructed in #{a1} µs, lookup in #{a2} µs")
    {b1, b2, bd, bm} = time(seq, Bst)
    IO.puts("BST tree of max depth #{bm} and average depth #{bd/n} - constructed in #{b1} µs, lookup in #{b2} µs")
  end
  defp time(seq, module) do
    empty = apply(module, :tree, [])
    {t1, inserted} = :timer.tc(fn -> List.foldl(seq, empty, fn(k, a) -> apply(module, :insert, [a, k, k]) end) end)
    {t2, total} = :timer.tc(fn -> List.foldl(seq, 0,
      fn(k, a) ->
        {:ok, d} = apply(module, :depth, [inserted, k])
        d + a
      end)
    end)
    max = apply(module, :max_depth, [inserted])
    {t1, t2, total, max}
  end

  defp sequence(0, _), do: []
  defp sequence(i, t), do: [:rand.uniform(t) | sequence(i - 1, t)]

end